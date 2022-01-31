/* SMOOCH */

var colorids = [];
var loaded = 0;

var Smooch = function(kissData) {

    this.set = new KissSet(kissData);
    this.set.update();
    this.set.draw();

    // This controls dragging and dropping.
    this.mouse = Mouser(this);

    return this;
};

var KissSet = function(kissData) {
    // Size of the play area.
    this.size = { x: kissData.window_size[0],
                  y: kissData.window_size[1] };

    // Set up border area (around the playarea)
    var borderarea = document.getElementById("borderarea");
    borderarea.style.background = kissData.border;

    // Set up play area
    var playarea = document.getElementById("playarea");
    playarea.style.width = this.size.x + "px";
    playarea.style.height = this.size.y + "px";
    playarea.style.background = kissData.background;

    // Set up canvases
    this.initCanvases();

    // Initalize click handler for set number listing
    this.initSetClicks();
    this.currentSet = 0;

    // Initialize objs and cels
    this.objs = [];
    this.cels = [];
    this.init(kissData.objs, kissData.cels);

    return this;
};

KissSet.prototype = {
    init: function (objs, cels) {
        /* Cels have to be kept in a separate list from the objects.
           This is because objects are the things that have click handlers,
           get dragged, change position, etc. But cels are the things that
           are drawn, and they have to be drawn in a certain order to preserve
           the layering effect. */

        // Helper function for matching objects and cels
        var matches = function(obj_cel, cel) {
            var unmatched = !obj_cel.matched;
            var name_matches = obj_cel.name === cel.name;
            var pal_matches = obj_cel.palette === cel.palette;
            return unmatched && name_matches && pal_matches;
        };



        /* Go through each KiSS object, add information from the object to the
           cels within the object, then add those cels to the list. */

        var red = 0;
        var blue = 0;
        var green = 0;

        for (var i = 0; i < objs.length; i++) {
            // create a unique color for each obj based on the obj id
            // and register it in the global colorids array
            // supports up to 255*3 objects
            if (i < 255){
                red = i;
            }
            else if ((i > 255) && (i < 255*2)){
                red = 0;
                green = i;
            }
            else if ((i > 255*2) && (i < 255*3)){
                green = 0;
                blue = i;
            }

            var colorid = red + green + blue + 255;
            colorids[colorid] = i;
            objs[i].color = { red: red, green: green, blue: blue, alpha: 255 };


            // now lets go through the cels
            var obj_cels = objs[i].cels;
            // for each cel in the obj, find that cel in the celData list
            for (var j = 0; j < obj_cels.length; j++) {
                for (var k = 0; k < cels.length; k++) {
                    // The only way we can match cels is by name.
                    // If multiple objects share the same cel, the cel list has
                    // multiple copies of the cel, and we skip cels that are already
                    // matched. This is hacky as hell.
                    // TODO: This is going to cause trouble if we have multiple cels with
                    // the same name, but they have different palettes applied.
                    // Could make sure that the palette of the object and the cel match
                    if (matches(obj_cels[j], cels[k], this.cels[k])) {
                        if (this.cels[k] && this.cels[k].obj) {
                            console.log("already matched");
                        } else {
                            obj_cels[j] = new KiSSCel(objs[i], cels[k], this);
                            this.cels[k] = obj_cels[j];
                            obj_cels[j].matched = true;
                        }
                    }
                }
            }

            this.objs[i] = new KiSSObj(objs[i]);
        }

        // cels have to be drawn in reverse order (drawing lowest items first)
        this.cels.reverse();
    },

    initCanvases: function() {
        var drawcanvas = document.getElementById("screen");
        var drawctxt = drawcanvas.getContext("2d");
        drawcanvas.style.width = this.size.x + "px";
        drawcanvas.style.height = this.size.y + "px";
        drawcanvas.width = this.size.x;
        drawcanvas.height = this.size.y;

        var ghostcanvas = document.getElementById("ghost");
        var ghostctxt = ghostcanvas.getContext("2d");
        ghostcanvas.style.width  = (this.size.x) + "px";
        ghostcanvas.style.height = (this.size.y) + "px";
        ghostcanvas.width = this.size.x;
        ghostcanvas.height = this.size.y;
        ghostcanvas.style.background = "blue";
        ghostcanvas.style.display = "none";

        this.ctxt = drawctxt;
        this.canvas = drawcanvas;
        this.ghost = ghostctxt;
    },

    initSetClicks: function () {
        // Add click events to set numbers
        this.sets = document.getElementsByTagName("a");
        for (var i = 0; i < this.sets.length; i++) {
            var that = this;
            // when a number is clicked, set current set to that number, update
            this.sets[i].addEventListener('click', function() {
                that.currentSet = parseInt(this.innerHTML);
                that.update();
                that.draw(this.ctxt, this.ghost);
            });
        }
    },

    update: function () {
        // Update cels
        for (var i = 0; i < this.objs.length; i++) {
            this.objs[i].update(this);
        }

        // Update set listing
        for (var i = 0; i < this.sets.length; i++) {
            if (this.currentSet == parseInt(this.sets[i].innerHTML)) {
                this.sets[i].style.color = "black";
            }
            else {
                this.sets[i].style.color = "grey";
            }
        }
    },

    draw: function() {
        this.ctxt.clearRect(0, 0, this.size.x, this.size.y);
        this.ghost.clearRect(0, 0, this.size.x, this.size.y);
        for (var i = 0; i < this.cels.length; i++) {
            if (this.cels[i]) {
                this.cels[i].draw(this.ctxt, this.ghost);

            }
        }
    }
};

var KiSSObj = function (obj) {
    this.currentSet = 0;
    this.positions = obj.positions;
    this.position = obj.positions[this.currentSet];
    this.cels = obj.cels;

    return this;
};

KiSSObj.prototype = {
    update: function(that) {
        for (var i = 0; i < this.cels.length; i++) {
            this.cels[i].currentSet = that.currentSet;
            this.cels[i].position = this.positions[that.currentSet];
            this.cels[i].update(that);
        }
    }
};

var KiSSCel = function(obj, cel, set) {

    this.obj = obj;
    this.name = cel.name;
    this.mark = obj.id;
    this.fix = cel.fix;
    this.position = obj.positions[0];
    this.positions = obj.positions;
    this.sets = cel.sets;
    this.image = undefined;
    this.ghostImage = undefined;
    this.visible = false;
    this.alpha = cel.alpha;

    this.offset = cel.offset;

    this.init(set);

    return this;
};

KiSSCel.prototype = {

    init: function(set) {
        var drawctxt = set.ctxt;
        var drawcanvas = set.canvas;
        var ghostctxt = set.ghost;

        var image = document.getElementById(this.name);
        var width = image.width;
        var height = image.height;

        this.image = image;

        // Draw image to ctxt and get image data
        drawctxt.drawImage(image,
                           0,
                           0,
                           width,
                           height);

        var ghostImageData = drawctxt.getImageData(0,0, width, height);
        var data = ghostImageData.data;

        // Fill image data with obj color
        var color = this.obj.color;

        for (var k = 0; k < data.length; k=k+4) {
            data[k]   = color.red;
            data[k+1] = color.green;
            data[k+2] = color.blue;
        }

        // Clear ctxt and draw altered image
        drawctxt.clearRect(0, 0, set.size.x, set.size.y);
        drawctxt.putImageData(ghostImageData, 0, 0);

        // Save altered image as cel's ghost image
        this.ghostImage = new Image();
        this.ghostImage.src = drawcanvas.toDataURL('image/png');

        // Let Smooch know when image is loaded
        this.ghostImage.onload = function() {
            loaded = loaded + 1;
        };

        // Clear ctxt
        drawctxt.clearRect(0, 0, set.size.x, set.size.y);

    },

    update: function(that) {
        if (this.sets.indexOf(that.currentSet) == -1) {
            this.visible = false;
        }
        else {
            this.visible = true;
        }
        if (this.name == "blink") {
            this.visible = false;
        }
    },

    draw: function (screen, ghost) {
        if (this.visible == true) {
            if (this.alpha) {
                screen.globalAlpha = (255-this.alpha)/255;
            }

            screen.drawImage(this.image,
                             this.position.x + this.offset.x,
                             this.position.y + this.offset.y);

            screen.globalAlpha = 1;

            ghost.drawImage(this.ghostImage,
                            this.position.x + this.offset.x,
                            this.position.y + this.offset.y);
        }
    }
};

var Mouser = function(that) {

    // This is from eLouai, believe it or not!!
    // http://www.elouai.com/javascript-drag-and-drop.php
    var isdrag = false;
    var x, y, dobj;

    var mousemove = function(e) {
        if (isdrag) {
            dobj.positions[curSet].x = tx + e.layerX - x;
            dobj.positions[curSet].y = ty + e.layerY - y;
            that.set.update(that.set);
            that.set.draw(that.set.ctxt, that.set.ghost);
            return false;
        } else {
            return true;
        }
    };

    // from http://www.html5canvastutorials.com/advanced/html5-canvas-mouse-coordinates/
    var getMousePos = function(canvas, evt) {
        var rect = canvas.getBoundingClientRect();
        return {
            x: evt.clientX - rect.left,
            y: evt.clientY - rect.top
        };
    };

    var selectmouse = function(e) {

        var screen = document.getElementById('screen');
        var canvas = document.getElementById('ghost');
        var ctxt = canvas.getContext('2d');

        var pos = getMousePos(screen, e);

        var pixel = ctxt.getImageData(pos.x, pos.y, 1, 1);

        var data = pixel.data;

        var colorid = data[0] + data[1] + data[2] + 255;

        if (data[3] === 0) {
            console.log("not draggable");
            return true;
        }
        else {
            var kobj = that.set.objs[colorids[colorid]];
            if (kobj && kobj.cels[0].fix < 1) {
                isdrag = true;
                dobj = kobj;
                var curSet = that.set.currentSet;
                tx = dobj.positions[curSet].x;
                ty = dobj.positions[curSet].y;
                x = e.layerX;
                y = e.layerY;
                document.onmousemove = mousemove;
                return false;
            } else {
                return true;
            }
        }
    };

    document.onmousedown = selectmouse;
    document.onmouseup = function () {
        isdrag = false;
    };

    this.getSelected = function() {
        return this.selectedObj;
    };

};

window.addEventListener('load', function() {
    var kissData = kissJson;
    this.smooch = new Smooch(kissData);
    var checkLoaded = function () {
        if (loaded < kissData.cels.length) {
            console.log("loading...");
            window.setTimeout(checkLoaded, 500);
        } else {
            this.smooch.set.draw();
        }
    };

    window.setTimeout(checkLoaded, 500);
});
