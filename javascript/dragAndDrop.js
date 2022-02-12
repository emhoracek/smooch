
class DragAndDrop {
  constructor () {
    this.dragHandler = false
    this.screen = document.getElementById('screen')
    this.canvas = document.getElementById('ghost')
    this.ctxt = this.canvas.getContext('2d')
  }

  initialize (doll) {
    document.addEventListener('mousedown', (e) => this.onMouseDown(e, doll))
    document.addEventListener('mouseup', () => {
      if (this.dragHandler) {
        document.removeEventListener('mousemove', this.dragHandler)
        this.dragHandler = false
      }
    })
  }

  onMouseDown (e, doll) {
    const pos = getMousePos(this.screen, e)
    const pixel = this.ctxt.getImageData(pos.x, pos.y, 1, 1)
    const data = pixel.data
    const colorid = data[0] + data[1] + data[2] + 255

    if (data[3] === 0) {
      console.log('not draggable')
    } else {
      const objIndex = doll.colorids[colorid]
      const obj = doll.objs[objIndex]
      if (obj && obj.cels[0].fix < 1) {
        const curSet = doll.currentSet
        const dragStart = toPositions(pos, obj, curSet)
        this.dragHandler = (e) => onMouseMove(e, this.screen, obj, dragStart, doll)
        document.addEventListener('mousemove', this.dragHandler)
        e.preventDefault()
      }
    }
  }
}

function toPositions (pos, obj, curSet) {
  const positions = { }
  positions.x = obj.positions[curSet].x - pos.x
  positions.y = obj.positions[curSet].y - pos.y
  return positions
}

function onMouseMove (e, canvas, obj, dragStart, doll) {
  const currentMousePosition = getMousePos(canvas, e)
  const x = dragStart.x + currentMousePosition.x
  const y = dragStart.y + currentMousePosition.y

  obj.setPosition(doll.currentSet, x, y)

  doll.update()
  doll.draw()

  e.preventDefault()
}

// from http://www.html5canvastutorials.com/advanced/html5-canvas-mouse-coordinates/
const getMousePos = function (canvas, evt) {
  const rect = canvas.getBoundingClientRect()
  return {
    x: evt.clientX - rect.left,
    y: evt.clientY - rect.top
  }
}

export { DragAndDrop }
