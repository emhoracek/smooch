<!DOCTYPE html>

<html>
  <head>
    <title>
      User page
    </title>
  </head>

  <body>

    <h1>Welcome, <username /></h1>

    <h2>Your sets</h2>

    <!-- Contirbutors: the bind tag below is a weird hack to get around a
         a limitation in the Larceny templating system. -->
    <bind tag="any-sets"><sets><setName /></sets></bind>

    <if exists="${any-sets}">
      <then>
        <ul>
          <sets>
            <li><a href="sets/${setName}"><setName /></a></li>
          </sets>
        </ul>
      </then>
      <else>
        <p>You don't have any sets, so try uploading one.</p>
      </else>
    </if>

    <h2>Upload a set</h2>

    <form action="/upload" method="post" enctype="multipart/form-data">

      <input type="file" name="kissfile">
      <input type="submit" value="Upload!">

    </form>

  </body>
</html>
