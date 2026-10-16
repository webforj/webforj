package com.webforj.bbjship.foundation;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Optional;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

final class PublishNamePolicyTest {

  @ParameterizedTest
  @ValueSource(strings = {"hello-app", "HelloApp", "hello_app.v2", "9lives", "console", "nul-app",
      "com0", "lpt10", "auxiliary.app", "applications", "helpdesk", "hello.json"})
  void shouldAcceptPublishName(String name) {
    assertTrue(PublishNamePolicy.isValid(name));
    assertFalse(PublishNamePolicy.isReserved(name));
  }

  @ParameterizedTest
  @NullAndEmptySource
  @ValueSource(
      strings = {".", "..", "../hello-app", "hello/app", "hello\\app", " hello-app", "-hello-app",
          ".hello-app", "_hello-app", "hello app", "hello:app", "héllo", "hello.", "hello-app.."})
  void shouldRefuseNameThatIsNoSafeFolderName(String name) {
    assertFalse(PublishNamePolicy.isValid(name));
  }

  @ParameterizedTest
  @ValueSource(strings = {"apps", "APPS", "webapp", "servlet", "webservice", "webconfig", "files",
      "buires", "gwtwebclient", "webapprmi", "fileDownloadServlet", "fileuploadservlet",
      "imageServlet", "printServlet", "basis", "dwcembed", "embedbui", "Help", "hello.jnlp",
      "hello.css", "hello.JS", "hello.cache.html", "hello.cache.png", "hello.gwt.rpc"})
  void shouldRefuseNameThatBbjServicesReservesForItsWebPaths(String name) {
    assertFalse(PublishNamePolicy.isValid(name));
    assertTrue(PublishNamePolicy.isReserved(name));
  }

  @ParameterizedTest
  @ValueSource(strings = {"con", "CON", "prn", "Aux", "nul", "com1", "COM9", "lpt1", "lpt9",
      "con.txt", "NUL.tar.gz", "com5.app"})
  void shouldRefuseReservedDeviceNameInAnyCaseAndWithAnyExtension(String name) {
    assertFalse(PublishNamePolicy.isValid(name));
  }

  @ParameterizedTest
  @ValueSource(strings = {"hello-app", "HelloApp", "hello_app.v2"})
  void shouldFindNoRefusalOfValidPublishName(String name) {
    assertEquals(Optional.empty(), PublishNamePolicy.findRefusal(name));
  }

  @ParameterizedTest
  @ValueSource(strings = {"My App", "../escaped", "con", "hello."})
  void shouldNameTheInvalidPublishNameInTheRefusal(String name) {
    assertEquals(Optional.of("Publish name " + name + " is invalid. Use a name that starts with a "
        + "letter or digit, holds only letters, digits, underscores, dots and hyphens, and is not "
        + "a reserved device name such as con or nul"), PublishNamePolicy.findRefusal(name));
  }

  @ParameterizedTest
  @ValueSource(strings = {"webapp", "Help", "hello.js"})
  void shouldNameTheReservationInTheRefusalOfReservedPublishName(String name) {
    assertEquals(Optional.of("The publish name " + name + " is reserved by BBjServices for its own "
        + "web paths. Choose another publish name"), PublishNamePolicy.findRefusal(name));
  }

  @ParameterizedTest
  @ValueSource(strings = {"", " "})
  void shouldAskForThePublishNameInTheRefusalOfBlankName(String name) {
    assertEquals(
        Optional.of("BBj deployment requires bbj.publishName or a final name of the build"),
        PublishNamePolicy.findRefusal(name));
  }
}
