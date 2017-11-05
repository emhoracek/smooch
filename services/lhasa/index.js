var promisify = require('promisify-node');

var fs = promisify('fs');
var spawn = require('child-process-promise').spawn;
var AWS = require('aws-sdk');

//var credentials = new AWS.SharedIniFileCredentials({profile: 'smooch'});
//AWS.config.credentials = credentials;

var set_file = "/tmp/set.lzh";
var set_dir = "/tmp/set";

// Make sure you append the path with the lambda deployment path, so you can
// execute your own binaries in, say, bin/
// (This is from https://gist.github.com/rayh/1ee0f6ce54cc9fafbb06)
process.env["PATH"] = process.env["PATH"] + ":" + process.env["LAMBDA_TASK_ROOT"];

function process_archive(event, callback) {
    var srcBucket = event.Records[0].s3.bucket.name;
    var srcKey =
        decodeURIComponent(event.Records[0].s3.object.key.replace(/\+/g, " "));

    var bucket_params = {
        Bucket: srcBucket,
        Key: srcKey
    };

    download(bucket_params)
        .then(unzip)
        .then(delete_archive)
        .then(removeDirRecursive)
        .then(function() {
            return callback(null, "yay!");
        })
        .catch(function (err) {
            return callback(err);
        });
}

// Download the image from S3, transform, and upload to a different S3 bucket.
function download(bucket_params) {
    var s3 = new AWS.S3();
    var writeStream = fs.createWriteStream(set_file);

    console.log("downloading lzh");

    // Download the archive from S3 into a buffer.
    var getArchive = s3.getObject(bucket_params).createReadStream();
    getArchive.pipe(writeStream);

    return new Promise(function(resolve, reject) {
        getArchive.on("finish", resolve);
        getArchive.on("error", reject);
    });
}

// Use lha to decompress the archive
function unzip() {
    return spawnCmd("lha", ["-xw=" + set_dir, set_file], {
        cwd: '/tmp'
    });
};

function delete_archive(result) {
    return fs.unlink(set_file);
}

function removeDirRecursive() {
    console.log("removing directory at ", set_dir + '/');
    return fs.readdir(set_dir + '/').then(function(files) {
        return Promise.all(files.map(function(file) {
            return deleteFileOrDirectory(file);
        }));
    }).then(function(result) {
            fs.rmdir(set_dir);
    });
}

function deleteFileOrDirectory(file) {
    var path = set_dir + '/' + file;
    return fs.stat(path).then(function(stats) {
        if (stats.isDirectory()) {
            return removeDirRecursive(path + '/');
        } else {
            return fs.unlink(path);
        }
    });
}

// function heavily modified from https://gist.github.com/rayh/1ee0f6ce54cc9fafbb06
function spawnCmd(cmd, args, opts) {
    var opts = opts||{};
    console.log("[spawn]", cmd, args.join(' '), opts);

    var cmd_promise = spawn(cmd, args, opts);
    var child = cmd_promise.childProcess;

    child.stdout.on('data', function(chunk) {
        console.log("[" + cmd + ":stdout] " + chunk);
    });

    child.stderr.on('data', function(chunk) {
        console.log("[" + cmd + ":stderr] " + chunk);
    });

    return cmd_promise;
}

exports.handler = function(event, context, callback) {
    console.log('Received event:', JSON.stringify(event, null, 2));
    process_archive(event, callback);
};
