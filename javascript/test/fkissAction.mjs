/* eslint-disable no-unused-expressions */
/* globals describe, it */
import { expect } from 'chai'
import { makeAction, objOrCelArg } from '../fkissAction.mjs'
import { Logger } from '../logger.mjs'

const testObj = {
  altmap () { return 'altmapped' },
  map () { return 'mapped' },
  setTransparency (n) { return `set alpha to ${n}` },
  unmap () { return 'unmapped' }
}
const testCel = {
  altmap () { return 'altmapped' },
  map () { return 'mapped' },
  setTransparency (n) { return `set alpha to ${n}` },
  unmap () { return 'unmapped' }
}
const testTimer = { timeout: 0 }
const testDoll = {
  changeSet (n) { return `set changed to ${n}` },
  getObject () { return testObj },
  getCel () { return testCel },
  getSound (soundFile) { return { play () { return `sound ${soundFile} played` } } },
  timers: [testTimer],
  setTimer (alarmId, duration) { return `timer ${alarmId} set for ${duration} ms` },
  logger: new Logger('debug')
}

describe('makeAction', function () {
  describe('altmap', function () {
    it('should work on objects', function () {
      const doll = testDoll
      const action = { action: 'altmap', args: [1] }

      const result = makeAction(action, doll)

      expect(result()).to.equal('altmapped')
    })
    it('should work on cels', function () {
      const doll = testDoll
      const action = { action: 'altmap', args: ['somecel.cel'] }

      const result = makeAction(action, doll)

      expect(result()).to.equal('altmapped')
    })
  })

  describe('changeset', function () {
    it("should change the doll's current set", function () {
      const doll = testDoll
      const action = { action: 'changeset', args: [9] }

      const result = makeAction(action, doll)

      expect(result()).to.equal('set changed to 9')
    })
  })

  describe('map', function () {
    it('should work for objects', function () {
      const doll = testDoll
      const action = { action: 'map', args: [1] }

      const result = makeAction(action, doll)

      expect(result()).to.equal('mapped')
    })
    it('should work for cels', function () {
      const doll = testDoll
      const action = { action: 'map', args: ['somecel.cel'] }

      const result = makeAction(action, doll)

      expect(result()).to.equal('mapped')
    })
  })

  describe('nop', function () {
    it("should't do anything", function () {
      const doll = testDoll
      const action = { action: 'nop', args: [] }

      const result = makeAction(action, doll)

      expect(result()).to.be.undefined
    })
  })

  describe('sound', function () {
    it('should work for wav files', function () {
      const doll = testDoll
      const action = { action: 'sound', args: ['some-sound.wav'] }

      const result = makeAction(action, doll)

      expect(result()).to.equal('sound some-sound.wav played')
    })

    it('should rename au files to wav', function () {
      const doll = testDoll
      const action = { action: 'sound', args: ['some-sound.au'] }

      const result = makeAction(action, doll)

      expect(result()).to.equal('sound some-sound.wav played')
    })
  })

  describe('transparent', function () {
    it('should work for objects', function () {
      const doll = testDoll
      const action = { action: 'transparent', args: [1, 125] }

      const result = makeAction(action, doll)

      expect(result()).to.equal('set alpha to 125')
    })

    it('should work for cels', function () {
      const doll = testDoll
      const action = { action: 'transparent', args: ['some.cel', 125] }

      const result = makeAction(action, doll)

      expect(result()).to.equal('set alpha to 125')
    })
  })

  describe('timer', function () {
    it('should take two integer arguments', function () {
      const doll = testDoll
      const action = { action: 'timer', args: [0, 100] }

      const result = makeAction(action, doll)

      expect(result()).to.equal('timer 0 set for 100 ms')
    })
  })

  describe('unmap', function () {
    it('should work for objects', function () {
      const doll = testDoll
      const action = { action: 'unmap', args: [1] }

      const result = makeAction(action, doll)

      expect(result()).to.equal('unmapped')
    })

    it('should work for cels', function () {
      const doll = testDoll
      const action = { action: 'unmap', args: ['some.cel'] }

      const result = makeAction(action, doll)

      expect(result()).to.equal('unmapped')
    })
  })
})

describe('objOrCelArg', function () {
  it('returns a cel if given a cel name', function () {
    const doll = testDoll
    const arg = 'test.cel'

    const result = objOrCelArg(arg, doll)

    expect(result).to.equal(testCel)
  })

  it('returns an object if given an object number', function () {
    const doll = testDoll
    const arg = 1

    const result = objOrCelArg(arg, doll)

    expect(result).to.equal(testObj)
  })

  it('returns undefined if given nonsense', function () {
    const doll = testDoll
    const arg = 'apple'

    const result = objOrCelArg(arg, doll)

    expect(result).to.be.undefined
  })
})
