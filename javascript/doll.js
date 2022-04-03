import { KiSSDoll } from './kissDoll.mjs'
import { DragAndDrop } from './dragAndDrop.mjs'

window.addEventListener('load', function () {
  /* globals globalKiss */
  let doll = false
  let loaded = 0

  const setDir = document.getElementById('set-data').dataset.staticDirectory

  fetch(`/sets/${setDir}/setdata.json`).then(resp => {
    return resp.json()
  }).then(kissJson => {
    const totalCels = kissJson.cels.length

    function checkLoaded () {
      console.log(`loading ${loaded} of ${totalCels}`)
      if (loaded < totalCels) {
        window.setTimeout(checkLoaded, 5)
      } else {
        doll.begin()
        document.getElementById('loading').style.display = 'none'
        document.getElementById('sets').style.display = 'block'
        document.getElementById('borderarea').style.display = 'block'
      }
    }

    window.setTimeout(checkLoaded, 5)

    doll = new KiSSDoll(kissJson)
    const dragger = new DragAndDrop(doll)
    doll.initialize(() => { loaded += 1 })
    dragger.initialize()

    // in testing and development, it's convenient to have access to
    // to the kiss set object in the development console
    if (typeof globalKiss !== 'undefined') {
      // eslint-disable-next-line no-global-assign
      globalKiss = doll
    }
  })
})
