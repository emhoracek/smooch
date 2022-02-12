
class KiSSCel {
  constructor (obj, cel, set) {
    this.obj = obj
    this.name = cel.name
    this.mark = obj.id
    this.fix = cel.fix
    this.position = obj.positions[0]
    this.positions = obj.positions
    this.sets = cel.sets
    this.image = undefined
    this.ghostImage = undefined
    this.visible = false
    this.alpha = cel.alpha

    this.offset = cel.offset

    this.init(set)

    return this
  }

  init (set, incLoaded) {
    this.incLoaded = incLoaded
    const drawctxt = set.ctxt
    const drawcanvas = set.canvas

    const image = document.getElementById(this.name)
    const width = image.width
    const height = image.height

    this.image = image

    // Draw image to ctxt and get image data
    drawctxt.drawImage(image, 0, 0, width, height)

    const ghostImageData = drawctxt.getImageData(0, 0, width, height)
    const data = ghostImageData.data

    // Fill image data with obj color
    const color = this.obj.color

    for (let k = 0; k < data.length; k = k + 4) {
      data[k] = color.red
      data[k + 1] = color.green
      data[k + 2] = color.blue
    }

    // Clear ctxt and draw altered image
    drawctxt.clearRect(0, 0, set.size.x, set.size.y)
    drawctxt.putImageData(ghostImageData, 0, 0)

    // Save altered image as cel's ghost image
    this.ghostImage = new Image()
    this.ghostImage.src = drawcanvas.toDataURL('image/png')

    // Let Smooch know when image is loaded
    this.ghostImage.onload = function () {
      this.incLoaded()
    }

    // Clear ctxt
    drawctxt.clearRect(0, 0, set.size.x, set.size.y)
  }

  update (currentSet) {
    if (this.sets.indexOf(currentSet) === -1) {
      this.visible = false
    } else {
      this.visible = true
    }
    if (this.name === 'blink') {
      this.visible = false
    }
  }

  draw (screen, ghost) {
    if (this.visible === true) {
      if (this.alpha) {
        screen.globalAlpha = (255 - this.alpha) / 255
      }

      screen.drawImage(
        this.image,
        this.position.x + this.offset.x,
        this.position.y + this.offset.y
      )

      screen.globalAlpha = 1

      ghost.drawImage(
        this.ghostImage,
        this.position.x + this.offset.x,
        this.position.y + this.offset.y
      )
    }
  }
}

export { KiSSCel }
