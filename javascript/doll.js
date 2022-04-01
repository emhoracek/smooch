import { KiSSDoll } from './kissDoll.mjs'
import { DragAndDrop } from './dragAndDrop.mjs'

window.addEventListener('load', function () {
  /* globals kissJson, globalKiss */
  let doll = false
  let loaded = 0
  const totalCels = kissJson.cels.length

  function checkLoaded () {
    console.log(`loading ${loaded} of ${totalCels}`)
    if (loaded < totalCels) {
      window.setTimeout(checkLoaded, 5)
    } else {
      document.getElementById('loading').style.display = 'none'
      document.getElementById('sets').style.display = 'block'
      document.getElementById('borderarea').style.display = 'block'
      doll.draw()
    }
  }

  window.setTimeout(checkLoaded, 5)

  doll = new KiSSDoll(kissJson)
  const dragger = new DragAndDrop(doll)
  doll.initialize(() => { loaded += 1 })
  dragger.initialize()

  // in testing and development, it's convenient to have access to
  // to the kiss set object
  if (typeof globalKiss !== 'undefined') {
    // eslint-disable-next-line no-global-assign
    globalKiss = doll
  }
})
