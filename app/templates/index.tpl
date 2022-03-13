<apply template="layout">
  <main>
    <apply template="_example_set"></apply>

    <section class="upload">
      <div>
        <h2>View a doll</h2>

        <h3>
          Be kind to <a href="http://otakuworld.com/kiss/">OtakuWorld</a>!
        </h3>

        <p>Bandwidth costs money and we want OtakuWorld to stay around
          for a long time.
        </p>

        <p>Instead of downloading dolls from OW directly, right
          click on the download link and select
          "Copy link". Paste the link below and Smooch will download it
          for you.
        </p>

        <form action="/dolls/upload" method="post">
          <div class="wide-field">
            <label for="link">Link:</label>
            <input name="link"
                   class="link-input"
                   pattern="https?:\/\/otakuworld.com\/data\/kiss\/data\/.*\.lzh"
                   title="Enter a valid OtakuWorld download URL"
                   placeholder="http://otakuworld.com/data/kiss/data/y/yura.lzh" />
            <input type="submit" value="View" class="field submit">
            <p><linkErrors /></p>
          </div>
        </form>

        <p>Already downloaded a doll? Upload the file below.</p>

        <form action="/dolls/upload" method="post" enctype="multipart/form-data">
          <input type="file" accept=".lzh" name="kissfile">
          <input type="submit" value="View" class="field submit" />
        </form>
      </div>
    </section>
  </main>
</apply>
