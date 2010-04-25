x = dlmread('spectrum.dat', ' ', 0, 1);
x = x(:,[(1:13),15]);
[wd, frac] = pca(x, 2);
xr = x*wd;
ws = scalem(xr, 'domain');
d = xr*ws;
scatterd(d);
dlmwrite('spectrum_pca.dat', d.data, 'delimiter', ' ');
system('cut -d" " -f1 spectrum.dat | paste -d " " - spectrum_pca.dat > feature.dat');
% ./dist/build/editfeature/editfeature mescaline.db insert Onset es.globero.mescaline.spectral 2 - < tools/feature.dat