#!/usr/bin/env bash
# Run as root on a Debian machine in order to install
# a multi-user nixpkg setup with 10 builders.

if [[ $USER != "root" ]]; then
    echo "Usage: sudo $0"
    exit 1
fi

NIX_VERSION="nix-1.11.9"

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

groupadd -r nixbld
for n in $(seq 1 10); do
    useradd -c "Nix build user $n" \
            -d /var/empty -g nixbld -G nixbld -M -N -r -s "$(which nologin)" \
            "nixbld$n"
done

wget http://nixos.org/releases/nix/$NIX_VERSION/$NIX_VERSION.tar.xz
tar -xvf $NIX_VERSION.tar.xz
cd "$NIX_VERSION" || exit 1
./configure --enable-gc
make -j 2
make install

cat << EOM > /etc/systemd/system/nix.service
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

cat << 'EOM' > /root/nix-setup-user.sh
nix-setup-user() {
    TARGET_USER="$1"
    SYMLINK_PATH="/home/$TARGET_USER/.nix-profile"
    PROFILE_DIR="/nix/var/nix/profiles/per-user/$TARGET_USER"

    echo "Creating profile $PROFILE_DIR..."
    echo "Profile symlink: $SYMLINK_PATH"

    rm "$SYMLINK_PATH"
    mkdir -p "$PROFILE_DIR"
    chown "$TARGET_USER:$TARGET_USER" "$PROFILE_DIR"

    ln -s "$PROFILE_DIR/profile" "$SYMLINK_PATH"
    chown -h "$TARGET_USER:$TARGET_USER" "$SYMLINK_PATH"

    echo "export NIX_REMOTE=daemon" >> "/home/$TARGET_USER/.bashrc"
    echo ". /usr/local/etc/profile.d/nix.sh" >> "/home/$TARGET_USER/.bashrc"

    su -lc "cd; . /usr/local/etc/profile.d/nix.sh; NIX_REMOTE=daemon nix-channel --update" "$TARGET_USER"
}
EOM

chmod +x /root/nix-setup-user.sh
cat "source /root/nix-setup-user.sh" >> /root/.bashrc

echo "Finished installing multi-user nixpkg."
echo "To enable a user, run nix-setup-user foouser"
