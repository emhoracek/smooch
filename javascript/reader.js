function readPaletteBlob() {
    var files = document.getElementById('pal-files').files;
    if (!files.length) {
        alert('Please select a file!');
        return;
    }

    var file = files[0];

    var reader = new FileReader();

	var cmarker;
    // If we use onloadend, we need to check the readyState.
    reader.onloadend = function(evt) {
        console.log("fire");
        if (evt.target.readyState == FileReader.DONE) { // DONE == 2
            var dv = new DataView(evt.target.result);
            var kmarker = "";
            for (var i = 0; i < 4; i++) {
                kmarker+= String.fromCharCode(dv.getUint8(i));
            }
            if (dv.getUint8(4) == 16) {
                cmarker = "Palette file marker found.";
            }
            else {
                cmarker = "Not a palette file.";
            }
            var bpc = dv.getInt8(5);
            var numcolors   = dv.getInt8(8) + (256 * dv.getInt8(9));
            var colors = [];

            for (var i = 0, off = 32; i < numcolors * 3; i++) {
                colors[i] = dv.getUint8(i+off);
            }

            document.getElementsByClassName('kiss-marker')[0].textContent = kmarker;
            document.getElementsByClassName('pal-marker')[0].textContent = cmarker;
            document.getElementsByClassName('bits-per-color')[0].textContent = bpc;
            document.getElementsByClassName('number-colors')[0].textContent = numcolors;

            readCelBlob(colors);
        }
    };

    reader.readAsArrayBuffer(file);
}

function readCelBlob(palette) {
    var files = document.getElementById('cel-files').files;
    if (!files.length) {
        alert('Please select a file!');
        return;
    }

    for (var i = 0; i < files.length; i++) {
	      var file = files[i];

	      var reader = new FileReader();

	      reader.onloadend = function(evt) {
	          if (evt.target.readyState == FileReader.DONE) { // DONE == 2
	              var dv = new DataView(evt.target.result);
	              var kmarker = "";
	              for (var i = 0; i < 4; i++) {
		                kmarker+= String.fromCharCode(dv.getUint8(i));
	              }
	              if (dv.getUint8(4) == 32) {
		                cmarker = "Cel file marker found.";
	              }
	              else {
		                cmarker = "Not a cel file.";
	              }
	              var width  = dv.getUint8(8)  + (256 * dv.getUint8(9));
	              var height = dv.getUint8(10) + (256 * dv.getUint8(11));
	              var offx   = dv.getUint8(12) + (256 * dv.getUint8(13));
	              var offy   = dv.getUint8(14) + (256 * dv.getUint8(15));

	              var pixels = new Uint8ClampedArray(width*height);
	              for (var i = 0, off = 32; i < (width * height); i++) {
		                pixels[i] = dv.getUint8(i+off);
	              }
	              var pixeldata = new Uint8ClampedArray(width * height * 4);
	              for (var i = 0, j = 0; i < width * height * 4; i = i + 4, j++) {
		                var palentry   = pixels[j] * 3;
		                pixeldata[i]   = palette[palentry];
		                pixeldata[i+1] = palette[palentry + 1];
		                pixeldata[i+2] = palette[palentry + 2];
		                if (palentry == 0) {
		                    pixeldata[i+3] = 0;
		                }
		                else {
		                    pixeldata[i+3] = 255;
		                }
	              }

	              var canvas = document.getElementsByClassName('cel-image')[0];
                var ctxt = canvas.getContext('2d');
                ctxt.clearRect(0, 0, screen.width, screen.height);

	              document.getElementsByClassName('kiss-marker')[0].textContent = kmarker;
	              document.getElementsByClassName('cel-marker')[0].textContent = cmarker;
	              document.getElementsByClassName('cel-width')[0].textContent = width;
	              document.getElementsByClassName('cel-height')[0].textContent = height;
	              document.getElementsByClassName('cel-offx')[0].textContent = offx;
	              document.getElementsByClassName('cel-offy')[0].textContent = offy;

	              var idata = new ImageData (pixeldata, width, height);
	              ctxt.putImageData(idata, 0,0);

	          }
	      };

	      reader.readAsArrayBuffer(file);
	  }
}
