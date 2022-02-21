import { KiSSCel } from './kissCel'
import { KiSSObject } from './kissObject'

class KiSSDoll {
  constructor (kissData, incLoaded) {
    // Size of the play area.
    this.size = { x: kissData.window_size[0], y: kissData.window_size[1] }

    // Set up border area (around the playarea)
    const borderarea = document.getElementById('borderarea')
    borderarea.style.background = kissData.border

    // Set up play area
    const playarea = document.getElementById('playarea')
    playarea.style.width = this.size.x + 'px'
    playarea.style.height = this.size.y + 'px'
    playarea.style.background = kissData.background

    // Set up canvases
    this.initCanvases(this.size)

    // Initialize current set
    this.currentSet = 0

    // Initialize objs and cels
    this.objs = []
    this.cels = []
    this.colorids = []
    this.init(kissData.cels, kissData.positions, incLoaded)
    initSetClicks(this)

    // Update and draw
    this.update()
    this.draw()

    return this
  }

  init (cnfCels, cnfPositions, incLoaded) {
    /* Cels have to be kept in a separate list from the objects.
        This is because objects are the things that get dragged, change
        position, etc. But cels are the things that are drawn, and they have
        to be drawn in a certain order to get the right layering effect. */

    cnfCels.reverse()

    cnfCels.forEach(cnfCel => {
      // Objects are indexed by their "mark". This is what groups cels together.
      // First check if there's already an object with that mark.
      const existingObj = this.objs[cnfCel.mark]
      if (existingObj) {
        // If object already exists, create a cel that points to that object.
        const newCel = new KiSSCel(existingObj, cnfCel, this, incLoaded)
        // Add the new cel to the object's list of cels.
        existingObj.cels.push(newCel)
        // Add the new cel to the doll's list of cels.
        this.cels.push(newCel)
      } else {
        // If the object doesn't already exist, we need to create it.

        // Create a color to identify the object.
        const color = numberToColor(cnfCel.mark)
        const colorid = color.red + color.green + color.blue + 255
        this.colorids[colorid] = cnfCel.mark

        // Create the new object and new cel
        const newObj = new KiSSObject(cnfCel.mark, color, cnfPositions.map(sp => sp.positions[cnfCel.mark]))
        const newCel = new KiSSCel(newObj, cnfCel, this, incLoaded)
        // Add the new cel to the object's list of cels.
        newObj.cels.push(newCel)
        // Add the object to the doll's list of objects.
        this.objs[cnfCel.mark] = newObj
        // Add the cel to the doll's list of cels.
        this.cels.push(newCel)
      }
    })
  }

  initCanvases (size) {
    const drawcanvas = document.getElementById('screen')
    setCanvasSize(drawcanvas, size)

    const ghostcanvas = document.getElementById('ghost')
    setCanvasSize(ghostcanvas, size)

    this.canvas = drawcanvas
    this.ctxt = drawcanvas.getContext('2d')
    this.ghost = ghostcanvas.getContext('2d')
  }

  getSelectedObject (pos) {
    const pixel = this.ghost.getImageData(pos.x, pos.y, 1, 1)
    const data = pixel.data
    const colorid = data[0] + data[1] + data[2] + 255
    const alpha = data[3]

    if (alpha === 0) {
      console.log('not draggable')
    } else {
      const objIndex = this.colorids[colorid]
      const obj = this.objs[objIndex]
      if (obj && obj.cels[0].fix < 1) {
        return obj
      }
    }
  }

  getObjectPosition (obj) {
    return obj.positions[this.currentSet]
  }

  moveObject (obj, x, y) {
    obj.setPosition(x, y)

    this.update()
    this.draw()
  }

  update (newSet) {
    // Update current set if new set is given
    if (typeof newSet !== 'undefined') { this.currentSet = newSet }

    // Update cels
    for (let i = 0; i < this.objs.length; i++) {
      // have to check if object exists first, because we are using
      // the object id ("mark") as the index for the array, so
      // many indexes are skipped.
      if (this.objs[i]) {
        this.objs[i].update(this.currentSet)
      }
    }
  }

  draw () {
    this.ctxt.clearRect(0, 0, this.size.x, this.size.y)
    this.ghost.clearRect(0, 0, this.size.x, this.size.y)
    for (let i = 0; i < this.cels.length; i++) {
      if (this.cels[i]) {
        this.cels[i].draw(this.ctxt, this.ghost)
      }
    }
  }
}

function initSetClicks (doll) {
  // Add click events to set numbers
  const sets = document.getElementsByTagName('a')
  for (let i = 0; i < sets.length; i++) {
    // when a number is clicked, update doll to new set
    sets[i].addEventListener('click', function () {
      const newSet = parseInt(this.innerHTML)
      doll.update(newSet)
      doll.draw()
      updateSets(doll.currentSet)
    })
  }
}

function updateSets (currentSet) {
  // Update set listing to highlight current set
  const sets = document.getElementsByTagName('a')
  for (let i = 0; i < sets.length; i++) {
    if (currentSet === parseInt(sets[i].innerHTML)) {
      sets[i].style.color = 'black'
    } else {
      sets[i].style.color = 'grey'
    }
  }
}

function setCanvasSize (canvas, size) {
  canvas.style.width = size.x + 'px'
  canvas.style.height = size.y + 'px'
  canvas.width = size.x
  canvas.height = size.y
}

function numberToColor (i) {
  /* Set color id initial values */
  let red = 0
  let blue = 0
  let green = 0

  // create a unique color for each object
  // and register it in the colorids array
  // supports up to 255*3 objects
  if (i < 255) {
    red = i
  } else if (i > 255 && i < 255 * 2) {
    red = 0
    green = i
  } else if (i > 255 * 2 && i < 255 * 3) {
    green = 0
    blue = i
  }

  return { red: red, green: green, blue: blue, alpha: 255 }
}

export { KiSSDoll }
