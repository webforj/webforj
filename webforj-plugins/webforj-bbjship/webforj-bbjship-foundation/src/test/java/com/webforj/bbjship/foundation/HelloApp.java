package com.webforj.bbjship.foundation;

import com.webforj.bbjship.foundation.ApplicationClasspath.ResolvedJar;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * The hello-app application fixture, packed as a BBj application build packs it.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class HelloApp {

  private static final String FIXTURE = "hello-app";
  private static final String EXCLUDED_ARTIFACT = "webforj-engine";
  private static final Pattern WEBFORJ_JAR =
      Pattern.compile("(webforj-[a-z]++(?:-[a-z]++)*+)-(\\d++(?:\\.\\d++)*+(?:-SNAPSHOT)?)\\.jar");

  private HelloApp() {}

  /**
   * Copies the fixture into a folder.
   *
   * @param target the folder to copy into
   *
   * @return the folder holding the copy
   * @throws IOException if the fixture cannot be read or the copy cannot be written
   */
  public static Path createCopy(Path target) throws IOException {
    return FixtureFolder.createCopy(FixtureFolder.getFolder(FIXTURE), target);
  }

  /**
   * Writes the application jar of a copy from its resources.
   *
   * @param app the folder holding the copy
   *
   * @return the application jar
   * @throws IOException if the resources cannot be read or the jar cannot be written
   */
  public static Path writeJar(Path app) throws IOException {
    Path resources = app.resolve("src/main/resources");
    Path jar = Files.createDirectories(app.resolve("target")).resolve("hello-app-1.0-SNAPSHOT.jar");
    try (JarOutputStream output = new JarOutputStream(Files.newOutputStream(jar));
        Stream<Path> files = Files.walk(resources)) {
      for (Path file : files.filter(Files::isRegularFile).sorted().toList()) {
        String name = resources.relativize(file).toString().replace(File.separatorChar, '/');
        output.putNextEntry(new JarEntry(name));
        Files.copy(file, output);
        output.closeEntry();
      }
    }

    return jar;
  }

  /**
   * Writes the deployment archive of a copy with the webforJ jars of the test classpath. The
   * webforj-engine jar is left out because the BBj server provides it.
   *
   * @param app the folder holding the copy
   * @param archive the archive to write
   *
   * @return the written archive
   * @throws IOException if the application jar or the archive cannot be written
   */
  public static Path writeArchive(Path app, Path archive) throws IOException {
    List<ResolvedJar> jars = new ArrayList<>();
    for (String entry : System.getProperty("java.class.path").split(File.pathSeparator)) {
      Path file = Path.of(entry);

      Matcher name = WEBFORJ_JAR.matcher(file.getFileName().toString());
      if (name.matches() && !EXCLUDED_ARTIFACT.equals(name.group(1))) {
        jars.add(new ResolvedJar(ApplicationClasspath.FRAMEWORK_GROUP_ID, name.group(1),
            name.group(2), file));
      }
    }

    return Deployment.create(archive).setApplication(writeJar(app))
        .setClasspath(new ApplicationClasspath(jars)).build().writeArchive();
  }

  /**
   * Gets a resource file of the original fixture.
   *
   * @param path the path relative to the resources folder of the fixture
   *
   * @return the file
   * @throws IOException if the fixture cannot be located
   */
  public static Path getFile(String path) throws IOException {
    return FixtureFolder.getFolder(FIXTURE).resolve("src/main/resources").resolve(path);
  }
}
