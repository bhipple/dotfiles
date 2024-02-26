{pkgs, ...}: {
  kernel.python.scientific.enable = true;
  kernel.python.scientific.extraPackages = ps: [
    ps.bokeh 
    ps.matplotlib
    ps.numpy 
    ps.pandas 
    ps.scipy 
    ps.seaborn
  ];
}
