function makeAction (a, doll) {
  const actionMap = {
    timer: mkTimer,
    altmap: mkAltmap,
    map: mkMap,
    unmap: mkUnmap
    /*
    changecol
    changeset
    move
    nop
    shell
    sound
    transparent
    unmap
    viewport
    windowsize
    */
  }

  const mkAction = actionMap[a.action]

  if (mkAction) {
    return mkAction(a.args, doll)
  }
  console.log('Unknown action', a.action)
}

function mkTimer (args, doll) {
  const alarmId = args[0]
  const duration = args[1]
  return () => setTimeout(doll.timers[alarmId], duration)
}

function mkMap (args, doll) {
  const objOrCel = objOrCelArg(args[0], doll)
  if (objOrCel) {
    return objOrCel.map.bind(objOrCel)
  }
}

function mkUnmap (args, doll) {
  const objOrCel = objOrCelArg(args[0], doll)
  if (objOrCel) {
    return objOrCel.unmap.bind(objOrCel)
  }
}

function mkAltmap (args, doll) {
  const objOrCel = objOrCelArg(args[0], doll)
  if (objOrCel) {
    return objOrCel.altmap.bind(objOrCel)
  }
}

function objOrCelArg (arg, doll) {
  if (typeof arg === 'number') {
    const obj = doll.getObject(arg)
    if (obj) {
      return obj
    } else {
      console.log('Unable to find object with mark', arg)
    }
  }
  if (arg.endsWith('.cel')) {
    const cel = doll.getCel(arg)
    if (cel) {
      return cel
    } else {
      console.log('Unable to find cel with filename', arg)
    }
  }
  console.log('Expected a cel or object reference but got: ', arg)
}

export { makeAction, objOrCelArg }
