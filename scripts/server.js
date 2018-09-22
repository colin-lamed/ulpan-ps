var express = require('express'),
app = express(),
port = process.env.PORT || 4000;

app.use('/ulpan-ps', express.static(__dirname + '/../public'));
app.listen(port);
