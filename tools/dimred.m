x = dlmread('spectrum.dat', ' ', 0, 1);
[wd, frac] = pca(x, 2);
ws = scalem(x*wd, 'domain');
d = x*wd*ws;
dlmwrite('spectrum_pca.dat', d.data, 'delimiter', ' ');
