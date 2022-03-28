/* eslint-disable no-unused-expressions */
/* globals describe, it */
import { expect } from 'chai'
import { makeAction } from '../fkissAction.mjs'

const testObj = {
  altmap () { return 'altmapped' },
  map () { return 'mapped' },
  unmap () { return 'unmapped' }
}
const testTimer = { timeout: 0 }
const testDoll = {
  getObject () { return testObj },
  getSound () { return { play () { return 'sound played' } } },
  timers: [testTimer],
  setTimer (alarmId, duration) { return `timer ${alarmId} set for ${duration} ms` }
}

describe('makeAction', function () {
  it('should work for altmap', function () {
    const doll = testDoll
    const action = { action: 'altmap', args: [1] }

    const result = makeAction(action, doll)

    expect(result()).to.equal('altmapped')
  })

  it('should work for map', function () {
    const doll = testDoll
    const action = { action: 'map', args: [1] }

    const result = makeAction(action, doll)

    expect(result()).to.equal('mapped')
  })

  it('should work for timer', function () {
    const doll = testDoll
    const action = { action: 'timer', args: [0, 100] }

    const result = makeAction(action, doll)

    expect(result()).to.equal('timer 0 set for 100 ms')
  })

  it('should work for timer', function () {
    const doll = testDoll
    const action = { action: 'sound', args: [1] }

    const result = makeAction(action, doll)

    expect(result()).to.equal('sound played')
  })

  it('should work for unmap', function () {
    const doll = testDoll
    const action = { action: 'unmap', args: [1] }

    const result = makeAction(action, doll)

    expect(result()).to.equal('unmapped')
  })
})

/*
describe('objOrCelArg', function () {
  it('returns a cel if given a cel name', function () {
    const doll = testDoll
    const arg = 'test.cel'

    const result = objOrCelArg(arg, doll)

    expect(result).not.to.be.undefined
  })

  it('returns an object if given a object number', function () {
    const doll = testDoll
    const arg = 1

    const result = objOrCelArg(arg, doll)

    expect(result).not.to.be.undefined
  })
})
*/
