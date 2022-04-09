<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8" />
    <title>Smooch</title>
    <link rel="stylesheet" type="text/css" href="/static/screen.css">
  </head>
  <body>
    <div id="wrapper">
      <div id="loading">
        <p>Loading...</p>
        <progress></progress>
      </div>

      <div id="sets">
        <ul>
          <set-listing>
            <li><button class="set"><set-number /></a></li>
          </set-listing>
          <li><button id="open-tip">?</a></li>
        </ul>
        <p class="tip">Click through the set by choosing a number. <button id="close-tip">Close</button></p>
      </div>

      <div id="borderarea">
        <div id="playarea">
            <canvas id="screen"></canvas>
            <canvas id="ghost"></canvas>
        </div>
      </div>

      <div id="set-data" data-static-directory="/${base}"></div>

      <div id="footer">
        <p>All sets are © their original artists. Learn more about Smooch <a href="https://github.com/emhoracek/smooch">from the Github repo</a>.</p>
      </div>
    </div>

    <script type="module" src="/static/doll.js"></script>
  </body>
</html>
