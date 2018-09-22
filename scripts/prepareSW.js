// https://github.com/GoogleChromeLabs/sw-precache

var swPrecache = require('sw-precache');
var rootDir = 'public';

// TODO font-awesome resources are requested with v?=4.7.0 so are cache misses
//      (addressed in FontAwesome 5?)
swPrecache.write(`${rootDir}/service-worker.js`, {
   cacheId: "ulpan-ps",
   staticFileGlobs: [rootDir + '/**/*.{js,html,css,png,jpg,gif,svg,eot,ttf,woff,json,yml}'],
   stripPrefix: rootDir + '/',
   verbose: true,
   maximumFileSizeToCacheInBytes: 4 * 1048576 // Main.js is 3.12 MB - larger than default of 2 MB
 });
