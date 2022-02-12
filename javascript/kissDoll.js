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
    this.init(kissData.objs, kissData.cels, incLoaded)
    initSetClicks(this)

    // Update and draw
    this.update()
    this.draw()

    return this
  }

  init (objs, cels, incLoaded) {
    /* Cels have to be kept in a separate list from the objects.
            This is because objects are the things that have click handlers,
            get dragged, change position, etc. But cels are the things that
            are drawn, and they have to be drawn in a certain order to preserve
            the layering effect. */

    // Helper function for matching objects and cels
    const matches = function (objCel, cel) {
      const unmatched = !objCel.matched
      const nameMatches = objCel.name === cel.name
      const palMatches = objCel.palette === cel.palette
      return unmatched && nameMatches && palMatches
    }

    /* Go through each KiSS object, add information from the object to the
            cels within the object, then add those cels to the list. */

    /* Set color id initial values */
    let red = 0
    let blue = 0
    let green = 0

    for (let i = 0; i < objs.length; i++) {
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

      const colorid = red + green + blue + 255
      this.colorids[colorid] = i
      objs[i].color = { red: red, green: green, blue: blue, alpha: 255 }

      // now lets go through the cels
      const objCels = objs[i].cels
      // for each cel in the obj, find that cel in the celData list
      for (let j = 0; j < objCels.length; j++) {
        for (let k = 0; k < cels.length; k++) {
          // The only way we can match cels is by name.
          // If multiple objects share the same cel, the cel list has
          // multiple copies of the cel, and we skip cels that are already
          // matched. This is hacky as hell.
          // TODO: This is going to cause trouble if we have multiple cels with
          // the same name, but they have different palettes applied.
          // Could make sure that the palette of the object and the cel match
          if (matches(objCels[j], cels[k], this.cels[k])) {
            if (this.cels[k] && this.cels[k].obj) {
              console.log('already matched')
            } else {
              objCels[j] = new KiSSCel(objs[i], cels[k], this, incLoaded)
              this.cels[k] = objCels[j]
              objCels[j].matched = true
            }
          }
        }
      }

      this.objs[i] = new KiSSObject(objs[i])
    }

    // cels have to be drawn in reverse order (drawing lowest items first)
    this.cels.reverse()
  }

  initCanvases (size) {
    const drawcanvas = document.getElementById('screen')
    const drawctxt = drawcanvas.getContext('2d')
    drawcanvas.style.width = size.x + 'px'
    drawcanvas.style.height = size.y + 'px'
    drawcanvas.width = size.x
    drawcanvas.height = size.y

    const ghostcanvas = document.getElementById('ghost')
    const ghostctxt = ghostcanvas.getContext('2d')
    ghostcanvas.style.width = size.x + 'px'
    ghostcanvas.style.height = size.y + 'px'
    ghostcanvas.width = size.x
    ghostcanvas.height = size.y
    ghostcanvas.style.background = 'blue'
    ghostcanvas.style.display = 'none'

    this.ctxt = drawctxt
    this.canvas = drawcanvas
    this.ghost = ghostctxt
  }

  update (newSet) {
    // Update current set if new set is given
    if (newSet) { this.currentSet = newSet }

    // Update cels
    for (let i = 0; i < this.objs.length; i++) {
      this.objs[i].update(this.currentSet)
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

export { KiSSDoll }