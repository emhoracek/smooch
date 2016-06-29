<html>
  <head>
    <meta charset="utf-8" />
    <title>Smooch</title>
    <link rel="stylesheet" type="text/css" href="/screen.css">
    <script type="text/javascript">
     window.onload = function () {
       document.getElementById('loading').style.display = 'none';
     };
    </script>
    <style> img { display: none; }</style>
  </head>
  <body>
    <div id="loading">
      <progress></progress>
    </div>

    <div id="sets">
      <ul>
        <set-listing>
          <li><a class="set"><set-number /></a></li>
        </set-listing>
      </ul>
    </div>

    <div id="borderarea">
      <div id="playarea">
           <canvas id="screen"></canvas>
           <canvas id="ghost"></canvas>
        <celImages />
        <canvas id="screen"></canvas>
        <canvas id="ghost"></canvas>
      </div>
    </div>


    <div id="nav">
      <button id="editbutton">Edit</button>
    </div>

    <div id="toolbar">
      <h1>Edit</h1>
      <div class="infobox">
      </div>
    </div>

    <div id="footer">From OtakuWorld.com</div>

    <script type="text/javascript" src="/${base}/setdata.js"></script>

    <script type="text/javascript" src="/doll.js"></script>

  </body>
</html>
