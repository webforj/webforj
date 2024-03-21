package com.webforj.installer.processor;

import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Objects;
import org.apache.commons.compress.archivers.zip.UnsupportedZipFeatureException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class ArchiveProcessorTest {

  private ClassLoader classLoader;

  @BeforeEach
  void setUp() {
    this.classLoader = getClass().getClassLoader();
  }

  @AfterEach
  void tearDown() {
  }


  @Test
  void test42(@TempDir File destDir) {
    File zipFile = getResourceFile("zip_bomb_resources/42.zip");
    assertThrows(UnsupportedZipFeatureException.class,
      () -> ArchiveProcessor.unzip(zipFile.getAbsolutePath(), destDir.toString()));

  }

  @Test
  void testGiantZipFile() {
    File zipFile = getResourceFile("zip_bomb_resources/gb_of_0s.zip");
    Path destFile = Path.of("/Users/kevin/foo.dat");
    assertThrows(IOException.class, () -> ArchiveProcessor.unzip(zipFile.getAbsolutePath(),
      destFile.toAbsolutePath().toString()));
  }


  @Test
  void testJar() throws Exception {
    ClassLoader classLoader = getClass().getClassLoader();
    File zipFile = new File(
      Objects.requireNonNull(classLoader.getResource("zip_bomb_resources/jars.zip")).getFile());
    Path destFile = Path.of("/Users/kevin/foo");
    System.out.println("zipFile = " + zipFile.exists());
    System.out.println("destFile = " + destFile);
    ArchiveProcessor.unzip(zipFile.getAbsolutePath(), destFile.toAbsolutePath().toString());
  }

  @Test
  void testZipSlipDetection() {
    ClassLoader classLoader = getClass().getClassLoader();
    File zipFile = new File(Objects
      .requireNonNull(classLoader.getResource("zip_bomb_resources/zipslip.zip")).getFile());
    Path destFile = Path.of("/Users/kevin/foo.dat");
    System.out.println("zipFile = " + zipFile.exists());
    System.out.println("destFile = " + destFile);
    assertThrows(IOException.class, () -> ArchiveProcessor.unzip(zipFile.getAbsolutePath(),
      destFile.toAbsolutePath().toString()));
  }


  /**
   * Commonly shared among the tests.
   *
   * @param resourcesPath where it is.
   * @return the file.
   */
  File getResourceFile(String resourcesPath) {
    return new File(Objects.requireNonNull(classLoader.getResource(resourcesPath)).getFile());
  }

}
