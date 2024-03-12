package installer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.installer.PomParser;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Map;
import java.util.Objects;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.xml.sax.SAXParseException;

class PomParserTest {

  private File pomFile;


  @BeforeEach
  void setUp() {
    ClassLoader classLoader = getClass().getClassLoader();
    pomFile = new File(Objects.requireNonNull(classLoader.getResource("pom.xml")).getFile());
  }

  @AfterEach
  void tearDown() {}


  @Test
  void testCanLoadPomFile() throws Exception {
    assertTrue(pomFile.exists());
    PomParser pomParser = new PomParser(pomFile.toString());
    Map<String, String> configuration = pomParser.parse();
    assertNotNull(configuration, "configuration returned null");
    assertFalse(configuration.isEmpty(), "expected 4 configuration children, was empty");
    assertEquals("hworld", configuration.get("publishname"));
    assertEquals("true", configuration.get("debug"));
    assertEquals("http://localhost:7888/dwcj-install", configuration.get("deployurl"));
    assertEquals("samples.HelloWorldJava", configuration.get("classname"));
  }

  @Test
  void test_valid_pom_no_configuration_children() throws Exception {
    ClassLoader classLoader = getClass().getClassLoader();
    File invalidPomFile = new File(Objects
        .requireNonNull(classLoader.getResource("pom_no_configuration_children.xml")).getFile());
    PomParser pomParser = new PomParser(invalidPomFile.toString());
    Map<String, String> configuration = pomParser.parse();
    assertNotNull(configuration, "configuration returned null");
    assertTrue(configuration.isEmpty(), "expected no configuration children, was not empty");
  }


  @Test
  void test_nonexistent_file_assert_exception() {
    String doesNotExist = "does not exist";

    PomParser pomParser = new PomParser(doesNotExist);
    IOException exception =
        assertThrows(IOException.class, pomParser::parse, "Expected IOException");
    assertInstanceOf(FileNotFoundException.class, exception.getCause(),
        "Expected FileNotFoundException as cause");
  }

  @Test
  void test_invalid_xml_assert_exception() {
    ClassLoader classLoader = getClass().getClassLoader();
    File invalidPomFile =
        new File(Objects.requireNonNull(classLoader.getResource("invalid_pom.xml")).getFile());
    PomParser pomParser = new PomParser(invalidPomFile.toString());
    IOException exception =
        assertThrows(IOException.class, pomParser::parse, "Expected Runtime Exception");
    assertInstanceOf(SAXParseException.class, exception.getCause(),
        "Expected SaxParseException as cause of error");
  }

}
