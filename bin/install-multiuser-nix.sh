#!/usr/bin/env bash
# Run as root on a Debian machine in order to install
# a multi-user nixpkg setup with 10 builders.

if [[ $USER != "root" ]]; then
    echo "Usage: sudo $0"
    exit 1
fi

NIX_VERSION="nix-1.11.9"
NIX_ARCHIVE="$NIX_VERSION.tar.xz"

install_prereqs() {
    if [ $(which yum) ]; then
        yum install flex bison perl-WWW-Curl perl-DBI perl-DBD-SQLite bzip2-devel
    else
        apt-get update && apt-get install -y \
                                  autotools-dev \
                                  build-essential \
                                  dh-autoreconf \
                                  libbz2-dev \
                                  libcurl4-openssl-dev \
                                  libdbd-sqlite3-perl \
                                  libdbi-perl \
                                  libgc-dev \
                                  liblzma-dev \
                                  libsqlite3-dev \
                                  libssl-dev \
                                  libwww-curl-perl \
                                  libxml2 \
                                  libxslt-dev \
                                  pkg-config
    fi
}

fetch_source() {
    wget http://nixos.org/releases/nix/$NIX_VERSION/$NIX_ARCHIVE{,.sha256}
    expected=$(cat "$NIX_ARCHIVE".sha256)
    actual=$(sha256sum "$NIX_ARCHIVE" | awk '{print $1}')
    if [ "$expected" != "$actual" ]; then
        echo "Downloaded invalid $NIX_ARCHIVE with sha256 $actual when I expected $expected"
        exit 1
    fi
    tar -xvf "$NIX_ARCHIVE"
}

build_from_source() {
    cd "$NIX_VERSION" || exit 1
    ./configure --enable-gc
    make -j 2
    make install
}

create_builders() {
    echo "Creating build daemon group and users"
    groupadd -r nixbld
    for n in $(seq 1 10); do
        useradd -c "Nix build user $n" \
                -d /var/empty -g nixbld -G nixbld -M -N -r -s "$(which nologin)" \
                "nixbld$n"
    done
}

setup_system() {
    echo "Setting up system profile environment variables"
    echo "export NIX_REMOTE=daemon" >> /etc/environment

    cat << 'EOM' > /etc/systemd/system/nix.service
[Unit]
Description=Nix daemon

[Service]
EnvironmentFile=-/etc/default/nix
ExecStart=/usr/local/bin/nix-daemon $EXTRA_OPTS
IgnoreSIGPIPE=false
KillMode=process

[Install]
WantedBy=multi-user.target
EOM

    touch /etc/default/nix

    systemctl enable nix
    systemctl start nix
}

create_nix_setup_script() {
    echo "Creating nix-setup-user.sh script"
    cat << 'EOM' > /root/nix-setup-user.sh
nix-setup-user() {
    TARGET_USER="$1"
    USER_HOME=$(getent passwd "$1" | cut -d : -f 6)
    SYMLINK_PATH="$USER_HOME/.nix-profile"
    PROFILE_DIR="/nix/var/nix/profiles/per-user/$TARGET_USER"

    echo "Creating profile $PROFILE_DIR..."
    echo "Profile symlink: $SYMLINK_PATH"

    rm -rf "$SYMLINK_PATH"
    mkdir -p "$PROFILE_DIR"
    chown "$TARGET_USER:$TARGET_USER" "$PROFILE_DIR"

    ln -s "$PROFILE_DIR/profile" "$SYMLINK_PATH"
    chown -h "$TARGET_USER:$TARGET_USER" "$SYMLINK_PATH"

    for p in bashrc zshrc; do
        echo "export NIX_REMOTE=daemon" >> "$USER_HOME/.$p"
        echo ". /usr/local/etc/profile.d/nix.sh" >> "$USER_HOME/.$p"
    done

    su -lc "cd; . /usr/local/etc/profile.d/nix.sh; NIX_REMOTE=daemon nix-channel --update" "$TARGET_USER"
}

if [ -z "$1" ]; then
    echo "Usage: $0 <user>"
    exit 1
fi

nix-setup-user $1
EOM

    chmod +x /root/nix-setup-user.sh
    echo "source /root/nix-setup-user.sh" >> /root/.bashrc
}

main() {
    install_prereqs
    build_from_source
    create_builders
    setup_system
    create_nix_setup_script

    echo "Finished installing multi-user nixpkg."
    echo "To enable a user, run nix-setup-user foouser"
}

main
