var fs = require('fs');
var Q = require('q');
var spawn = require('child_process').spawn;
var path = require('path');
var async = require('async');
var AWS = require('aws-sdk');
var _ = require('lodash/core');


var credentials = new AWS.SharedIniFileCredentials({profile: 'smooch'});
AWS.config.credentials = credentials;


var set_file = "/tmp/set.lzh";
var set_dir = "/tmp/set";

// Download the image from S3, transform, and upload to a different S3 bucket.
function download(event, context) {
    var s3 = new AWS.S3();
    var srcBucket = event.Records[0].s3.bucket.name;
    var srcKey =
        decodeURIComponent(event.Records[0].s3.object.key.replace(/\+/g, " "));

    var writeStream = fs.createWriteStream(set_file);

    console.log("downloading lzh");

    // Download the archive from S3 into a buffer.
    var getBlah = s3.getObject({
        Bucket: srcBucket,
        Key: srcKey
    }).createReadStream();

    getBlah.pipe(writeStream);

    getBlah.on("finish", function() { unzip(context); } );
};

function unzip(context) {
    // var root =  process.env["LAMBDA_TASK_ROOT"];

    console.log("unzipping set");
    fs.mkdir(set_dir);

    console.log("context", context);

    spawnCmd("lha", ["-xw=" + set_dir, set_file], {
        cwd: '/tmp'
    }).then(function(result) {
        fs.unlink(set_file);
        console.log("removing directory");
        removeDirForce(set_dir + "/", context);
    }, function(err) {
        context.fail(err);
    });
};

function removeDirForce(path, context) {
    fs.readdir(path, function(err, files) {
		    if (err) {
		        console.log(err.toString());
		    }
		    else {
            function deleteDirectory(err) {
                console.log("deleting directory");
                if (err) {
		                console.log(err.toString());
                    removeDirForce(path, context);
		            }
		            else {
			              fs.rmdir(path);
                    context.success("Everything's cleaned up!");
		            }
            }

            function deleteFile(file, callback) {
                filePath = path + file;
				        err = fs.unlinkSync(filePath);
                callback(err);
            }

			      async.eachSeries(files, deleteFile, deleteDirectory);
		    }
	  });
}

// Make sure you append the path with the lambda deployment path, so you can
// execute your own binaries in, say, bin/
process.env["PATH"] = process.env["PATH"] + ":" + process.env["LAMBDA_TASK_ROOT"];

function spawnCmd(cmd, args, opts) {
    var opts = opts||{};
    var basename = path.basename(cmd);

    console.log("[spawn]", cmd, args.join(' '), opts);

    var deferred = Q.defer();
    child = spawn(cmd, args, opts);
    child.stdout.on('data', function(chunk) {
        console.log("[" + basename + ":stdout] " + chunk);
    });

    child.stderr.on('data', function(chunk) {
        console.log("[" + basename + ":stderr] " + chunk);
    });
    child.on('error', function (error) {
        console.log("[" + basename + "] unhandled error:",error);
        deferred.reject(new Error(error));
    });
    child.on('close', function (code, signal) {
        if(signal) {
            deferred.reject("Process killed with signal " + signal);
        } else if(code==0) {
            deferred.resolve(code);
        } else {
            deferred.reject("Process exited with code " + code);
        }
        console.log("[" + basename + "] child process exited with code",code, "signal", signal);
    });

    return deferred.promise;
}

exports.handler = function(event, context) {
    console.log('Received event:', JSON.stringify(event, null, 2));
    download(event, context);
};
