<section class="demo">
  <div id="wrapper">
    <div id="loading">
      <p>Loading...</p>
      <progress></progress>
    </div>

    <div id="sets">
      <ul>
        <li><button class="set">0</button></li>
        <li><button class="set">1</button></li>
        <li><button class="set">2</button></li>
        <li><button class="set">3</button></li>
        <li><button class="set">4</button></li>
        <li><button class="set">5</button></li>
        <li><button class="set">6</button></li>
        <li><button class="set">7</button></li>
        <li><button class="set">8</button></li>
        <li><button class="set">9</button></li>
        <li><button id="open-tip">?</button></li>
      </ul>
      <p class="tip">Click through the set by choosing a number. <button id="close-tip">Close</button></p>
    </div>
    <!-- NOTE: I had to change the path, might have to think about making a partial to make it dynamic and not have to write a path -->
    <div id="borderarea">
      <div id="playarea">
        <canvas id="screen"></canvas>
        <canvas id="ghost"></canvas>
        <img src="/static/yura/palette0/yura.png" id="yura-0" /><img
          src="/static/yura/palette0/logo.png"
          id="logo-0"
        /><img src="/static/yura/palette0/guild.png" id="guild-0" /><img
          src="/static/yura/palette0/neck9b.png"
          id="neck9b-0"
        /><img src="/static/yura/palette0/neck6b.png" id="neck6b-0" /><img
          src="/static/yura/palette0/top20b.png"
          id="top20b-0"
        /><img src="/static/yura/palette0/vest1b.png" id="vest1b-0" /><img
          src="/static/yura/palette0/top15b.png"
          id="top15b-0"
        /><img src="/static/yura/palette0/swordb.png" id="swordb-0" /><img
          src="/static/yura/palette0/purse4b.png"
          id="purse4b-0"
        /><img src="/static/yura/palette0/purse3b.png" id="purse3b-0" /><img
          src="/static/yura/palette0/dress1b.png"
          id="dress1b-0"
        /><img src="/static/yura/palette0/top14b.png" id="top14b-0" /><img
          src="/static/yura/palette0/top13b.png"
          id="top13b-0"
        /><img src="/static/yura/palette0/top12b.png" id="top12b-0" /><img
          src="/static/yura/palette0/top11b.png"
          id="top11b-0"
        /><img src="/static/yura/palette0/skirt7b.png" id="skirt7b-0" /><img
          src="/static/yura/palette0/top9b.png"
          id="top9b-0"
        /><img src="/static/yura/palette0/top8b.png" id="top8b-0" /><img
          src="/static/yura/palette0/purse2b.png"
          id="purse2b-0"
        /><img src="/static/yura/palette0/top7b.png" id="top7b-0" /><img
          src="/static/yura/palette0/belt1b.png"
          id="belt1b-0"
        /><img src="/static/yura/palette0/top6b.png" id="top6b-0" /><img
          src="/static/yura/palette0/shoes5b.png"
          id="shoes5b-0"
        /><img src="/static/yura/palette0/purse1b.png" id="purse1b-0" /><img
          src="/static/yura/palette0/top5b.png"
          id="top5b-0"
        /><img src="/static/yura/palette0/top3b.png" id="top3b-0" /><img
          src="/static/yura/palette0/top1b.png"
          id="top1b-0"
        /><img src="/static/yura/palette0/top2b.png" id="top2b-0" /><img
          src="/static/yura/palette0/jean1b.png"
          id="jean1b-0"
        /><img src="/static/yura/palette0/myo.png" id="myo-0" /><img
          src="/static/yura/palette0/myo2.png"
          id="myo2-0"
        /><img src="/static/yura/palette0/und1.png" id="und1-0" /><img
          src="/static/yura/palette0/socks1.png"
          id="socks1-0"
        /><img src="/static/yura/palette0/socks4.png" id="socks4-0" /><img
          src="/static/yura/palette0/socks3.png"
          id="socks3-0"
        /><img src="/static/yura/palette0/socks2.png" id="socks2-0" /><img
          src="/static/yura/palette0/socks6.png"
          id="socks6-0"
        /><img src="/static/yura/palette0/socks5.png" id="socks5-0" /><img
          src="/static/yura/palette0/shoes4.png"
          id="shoes4-0"
        /><img src="/static/yura/palette0/shoes3.png" id="shoes3-0" /><img
          src="/static/yura/palette0/shoes2.png"
          id="shoes2-0"
        /><img src="/static/yura/palette0/shoes14.png" id="shoes14-0" /><img
          src="/static/yura/palette0/shoes13.png"
          id="shoes13-0"
        /><img src="/static/yura/palette0/shoes12.png" id="shoes12-0" /><img
          src="/static/yura/palette0/shoes11.png"
          id="shoes11-0"
        /><img src="/static/yura/palette0/shoes10.png" id="shoes10-0" /><img
          src="/static/yura/palette0/shoes8.png"
          id="shoes8-0"
        /><img src="/static/yura/palette0/shoes6.png" id="shoes6-0" /><img
          src="/static/yura/palette0/shoes5f.png"
          id="shoes5f-0"
        /><img src="/static/yura/palette0/shoes9.png" id="shoes9-0" /><img
          src="/static/yura/palette0/shoes15.png"
          id="shoes15-0"
        /><img src="/static/yura/palette0/shoes7.png" id="shoes7-0" /><img
          src="/static/yura/palette0/jean1f.png"
          id="jean1f-0"
        /><img src="/static/yura/palette0/shoes1.png" id="shoes1-0" /><img
          src="/static/yura/palette0/jean2.png"
          id="jean2-0"
        /><img src="/static/yura/palette0/dress2.png" id="dress2-0" /><img
          src="/static/yura/palette0/dress1f.png"
          id="dress1f-0"
        /><img src="/static/yura/palette0/skirt11.png" id="skirt11-0" /><img
          src="/static/yura/palette0/skirt10.png"
          id="skirt10-0"
        /><img src="/static/yura/palette0/skirt9.png" id="skirt9-0" /><img
          src="/static/yura/palette0/skirt8.png"
          id="skirt8-0"
        /><img src="/static/yura/palette0/skirt7f.png" id="skirt7f-0" /><img
          src="/static/yura/palette0/skirt1.png"
          id="skirt1-0"
        /><img src="/static/yura/palette0/skirt14.png" id="skirt14-0" /><img
          src="/static/yura/palette0/skirt18.png"
          id="skirt18-0"
        /><img src="/static/yura/palette0/skirt17.png" id="skirt17-0" /><img
          src="/static/yura/palette0/skirt16.png"
          id="skirt16-0"
        /><img src="/static/yura/palette0/skirt15.png" id="skirt15-0" /><img
          src="/static/yura/palette0/skirt12.png"
          id="skirt12-0"
        /><img src="/static/yura/palette0/skirt13.png" id="skirt13-0" /><img
          src="/static/yura/palette0/skirt5.png"
          id="skirt5-0"
        /><img src="/static/yura/palette0/skirt4.png" id="skirt4-0" /><img
          src="/static/yura/palette0/skirt2.png"
          id="skirt2-0"
        /><img src="/static/yura/palette0/skirt3.png" id="skirt3-0" /><img
          src="/static/yura/palette0/skirt6.png"
          id="skirt6-0"
        /><img src="/static/yura/palette0/arm1.png" id="arm1-0" /><img
          src="/static/yura/palette0/top2f.png"
          id="top2f-0"
        /><img src="/static/yura/palette0/top1f.png" id="top1f-0" /><img
          src="/static/yura/palette0/top8f.png"
          id="top8f-0"
        /><img src="/static/yura/palette0/top13f.png" id="top13f-0" /><img
          src="/static/yura/palette0/top15f.png"
          id="top15f-0"
        /><img src="/static/yura/palette0/top12f.png" id="top12f-0" /><img
          src="/static/yura/palette0/top10.png"
          id="top10-0"
        /><img src="/static/yura/palette0/top3f.png" id="top3f-0" /><img
          src="/static/yura/palette0/top14f.png"
          id="top14f-0"
        /><img src="/static/yura/palette0/top5f.png" id="top5f-0" /><img
          src="/static/yura/palette0/top19.png"
          id="top19-0"
        /><img src="/static/yura/palette0/top18.png" id="top18-0" /><img
          src="/static/yura/palette0/top17.png"
          id="top17-0"
        /><img src="/static/yura/palette0/top16.png" id="top16-0" /><img
          src="/static/yura/palette0/top7f.png"
          id="top7f-0"
        /><img src="/static/yura/palette0/vest1f.png" id="vest1f-0" /><img
          src="/static/yura/palette0/top20f.png"
          id="top20f-0"
        /><img src="/static/yura/palette0/top11f.png" id="top11f-0" /><img
          src="/static/yura/palette0/top4.png"
          id="top4-0"
        /><img src="/static/yura/palette0/top9f.png" id="top9f-0" /><img
          src="/static/yura/palette0/top6f.png"
          id="top6f-0"
        /><img src="/static/yura/palette0/neck1.png" id="neck1-0" /><img
          src="/static/yura/palette0/belt1f.png"
          id="belt1f-0"
        /><img src="/static/yura/palette0/belt2.png" id="belt2-0" /><img
          src="/static/yura/palette0/purse2f.png"
          id="purse2f-0"
        /><img src="/static/yura/palette0/purse4f.png" id="purse4f-0" /><img
          src="/static/yura/palette0/purse3f.png"
          id="purse3f-0"
        /><img src="/static/yura/palette0/purse1f.png" id="purse1f-0" /><img
          src="/static/yura/palette0/swordf.png"
          id="swordf-0"
        /><img src="/static/yura/palette0/neck2.png" id="neck2-0" /><img
          src="/static/yura/palette0/neck3.png"
          id="neck3-0"
        /><img src="/static/yura/palette0/neck7.png" id="neck7-0" /><img
          src="/static/yura/palette0/neck9f.png"
          id="neck9f-0"
        /><img src="/static/yura/palette0/neck6f.png" id="neck6f-0" /><img
          src="/static/yura/palette0/neck5.png"
          id="neck5-0"
        /><img src="/static/yura/palette0/neck4.png" id="neck4-0" /><img
          src="/static/yura/palette0/scarf2.png"
          id="scarf2-0"
        /><img src="/static/yura/palette0/scarf4.png" id="scarf4-0" /><img
          src="/static/yura/palette0/scarf3.png"
          id="scarf3-0"
        /><img src="/static/yura/palette0/scarf1.png" id="scarf1-0" /><img
          src="/static/yura/palette0/hair1.png"
          id="hair1-0"
        />
      </div>
    </div>
  </div>
</section>
