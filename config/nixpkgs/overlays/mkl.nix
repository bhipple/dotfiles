self: super: {
  # python = super.python.override {
  #   packageOverrides = python-self: python-super: {
  #     numpy = python-super.numpy.override {
  #       blas = super.pkgs.mkl;
  #     };
  #   };
  # };

  # python3 = super.python3.override {
  #   packageOverrides = python-self: python-super: {
  #     numpy = python-super.numpy.override {
  #       blas = super.pkgs.mkl;
  #     };
  #   };
  # };
}
