# Here for Hacktoberfest? 

Please comment on the issue you want to work on to claim it *before* you start
work. That way you can take your time working instead of racing to get the first
pull request in. Thank you!!

Don't see an interesting issue, or all the issues are claimed? 

Check out the [CONTRIBUTING](https://github.com/emhoracek/smooch/blob/master/CONTRIBUTING.md) file for ideas and more ways to contribute!

I'll be on [Twitch](https://www.twitch.tv/horrorcheck) working on this project most Sundays. 
Please feel free to ask questions in the chat there and let me know if you need any help getting the project running!

# smooch

Smooch is a set of tools for viewing, converting, and eventually creating KiSS
dolls. KiSS stands for Kisekae (Japanese for "dress up") Set System. 

KiSS dolls have been around for a long time, but there's only one modern viewer
(and it has some drawbacks) and no way to play with these dolls online. Many
very talented artists spent countless hours making these fun digital toys. Some
are wonderful examples of pixel artistry. It would be a shame for them to be
lost just because technology has moved on.

A typical KiSS doll: [Spark by Kimiki](http://otakuworld.com/kiss/dolls/pages/k/ki_spark.htm)

![Playing with a KiSS doll](http://i.imgur.com/UnxpRmL.gif)

Check out Smooch's [GitHub wiki](https://github.com/emhoracek/smooch/wiki) if you want to learn more about KiSS.

`cel2pnm` is a tool for converting KiSS cells (the individual graphics) to pnm
format (which is then simple to convert to lots of other formats). It should
work with all cel formats.

Now, we're making a KiSS doll viewer for the browser -- and you can help! Check
out the [CONTRIBUTING.md](https://github.com/emhoracek/smooch/blob/master/CONTRIBUTING.md) file for details.

![first-timers-only-friendly](http://img.shields.io/badge/first--timers--only-friendly-blue.svg?style=flat-square)

## Thanks

I (Libby/emhoracek/horrorcheck) started and maintain this project but it wouldn't have gotten where it is now without the efforts of many other people! That includes:

 * the international KiSS community
   * the Japanese KiSS community that developed KiSS as an open standard in the first place
   * Dov Sherman, who maintained the Big KiSS Page (an incredible resource) for many years
   * Stephan Lepisto, who has kept the Big KiSS Page going even after the community died out
   * Nick Lamb, who wrote GnomeKiSS, which I learned and copied a lot from for cel2pnm
   * William Miles, who wrote UltraKiSS, the Java app that I use as a benchmark for compatibility
   * all the incredible artists who made over 4,500 sets for this system
 * Github contributors
   * ear/bpaf helped with Haskell refactoring
   * Darius improved cel2pnm and added tests
   * huggablemonad has helped debug several sets
 * mentors
   * Daniel Patterson (dbp) taught me about monad transformers and error handling in Haskell
   * Mark Dominus helped me write cel2pmn

## Feedback

I would very much appreciate input on how to make this program better.

Please let me know if cel2pnm doesn't convert a cell correctly and I will
attempt to fix it.

Smooch should display any plain KiSS/GS doll correctly. FKiSS and Cherry KiSS
aren't supported yet. If you find a plain KiSS doll that Smooch doesn't display
right, please submit an issue along with an error message, so we can figure out why!

If you would like to help with this or other tools for KiSS, please let me know!
Email me at libby@daydrea.me, submit an issue, or send a pull request. THANKS!! :D
