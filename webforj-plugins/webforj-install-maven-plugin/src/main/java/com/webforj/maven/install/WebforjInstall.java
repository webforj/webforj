package com.webforj.maven.install;

import io.vavr.control.Try;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import org.apache.commons.lang3.StringUtils;
import org.apache.hc.client5.http.classic.methods.HttpPost;
import org.apache.hc.client5.http.entity.mime.FileBody;
import org.apache.hc.client5.http.entity.mime.HttpMultipartMode;
import org.apache.hc.client5.http.entity.mime.MultipartEntityBuilder;
import org.apache.hc.client5.http.impl.classic.HttpClients;
import org.apache.hc.core5.http.ContentType;
import org.apache.hc.core5.http.HttpEntity;
import org.apache.hc.core5.http.io.entity.EntityUtils;
import org.apache.hc.core5.http.message.StatusLine;
import org.apache.hc.core5.util.Args;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.apache.tika.Tika;
import org.apache.tika.mime.MimeTypes;


/**
 * WebforjInstall installs the jar in the target dir into BBj, communicating with the configured
 * deployurl, uploading the build.
 *
 * @author Stephan Wald
 * @author Kevin Hagel
 * @see <a href=
 *      "https://maven.apache.org/plugin-tools/maven-plugin-annotations/apidocs/org/apache/maven/plugins/annotations/Parameter.html">Annotation
 *      Type Parameter</a>
 * @see <a href="https://hc.apache.org/">Apache HttpComponents</a>
 * @see <a href="https://hc.apache.org/httpcomponents-core-5.2.x/">HttpCore</a>
 */
@Mojo(name = "install", defaultPhase = LifecyclePhase.INSTALL,
    requiresDependencyResolution = ResolutionScope.RUNTIME)
public class WebforjInstall extends AbstractMojo {

  /**
   * The current project instance.
   */
  @Parameter(defaultValue = "${project}", required = true, readonly = true)
  MavenProject project;

  /**
   * The destination url to use for deployment.
   */
  @Parameter(property = "deployurl", required = true)
  private String deployurl;

  /**
   * Optional username property for deployment.
   */
  @Parameter(property = "username")
  private String username;

  /**
   * Optional password property for the deployment.
   */
  @Parameter(property = "password")
  private String password;

  /**
   * Optional token property.
   */
  @Parameter(property = "token")
  private String token;

  /**
   * Inject the optional classname from the configuration.
   */
  @Parameter(property = "classname")
  private String classname;

  /**
   * Optional publishname property from the configuration.
   */
  @Parameter(property = "publishname")
  private String publishname;

  /**
   * Optional debug property from the configuration.
   */
  @Parameter(property = "debug")
  private String debug;

  /**
   * Optional parameter to put class files in classpath instead of JAR
   */
  @Parameter(property = "useclassfiles")
  private String useclassfiles;

  /**
   * Optional String pointing to the (in Docker potentially mapped) classes folder name
   */
  @Parameter(property = "classfolder")
  private String classfolder;

  /**
   * Optional watch parameter to enable hot reloading.
   */
  @Parameter(property = "watch")
  private String watch;


  /**
   * A private Tika instance.
   */
  private final Tika tika = new Tika();

  /**
   * The execute method called by maven.
   *
   * @throws MojoExecutionException when something fails.
   */
  public void execute() throws MojoExecutionException {
    getLog().info("Validating Mojo configuration ...");
    Args.check(deployurl != null, "deployurl is null!");
    Args.check(project != null, "project is null!");
    Args.check(project.getArtifact() != null, "project artifact is null!");
    final File file = Args.notEmpty(project.getArtifact().getFile(), "artifact file is null!");

    getLog().info("-------webforJ Deploy to Server:-------------");
    getLog().info("Installing webforJ App using URL: " + deployurl);

    Try.withResources(HttpClients::createDefault, () -> createMultipartEntity(file))
        .of((httpClient, reqEntity) -> {
          final HttpPost httpPost = new HttpPost(deployurl);
          httpPost.setEntity(reqEntity);

          // Executes a request using the default context and processes the response using the given
          // response handler. The content entity associated with the response is fully consumed and
          // the underlying connection is released back to the connection manager automatically in
          // all cases relieving individual HttpClientResponseHandlers from having to manage
          // resource deallocation internally.
          httpClient.execute(httpPost, response -> {
            getLog().info("----------------------------------------");
            getLog().info(httpPost + " response status -> " + new StatusLine(response));
            HttpEntity responseEntity = response.getEntity();
            String result = EntityUtils.toString(responseEntity);
            getLog().info(result);
            return result;
          });
          EntityUtils.consume(reqEntity);
          // Return value not used.
          return Try.success(null);
        })
        .onFailure(throwable -> getLog()
            .error("Error attempting deployment: " + throwable.getMessage(), throwable))
        .getOrElseThrow(MojoExecutionException::new);
  }

  /**
   * The {@link MultipartEntityBuilder} creates a builder whose build method is closeable.
   *
   * @param file the file being sent.
   * @return an HttpEntity created by using the builder
   * @throws IOException if we can't probe the content type of the file.
   */
  public HttpEntity createMultipartEntity(File file) throws IOException {
    getLog().info("creating multipart entity for file %s".formatted(file));
    String contentType = getContentType(file);
    getLog().info("discovered content type = %s".formatted(contentType));
    ContentType mimetype = ContentType.create(contentType);
    getLog().info("Installing file " + file + ", mimetype = " + mimetype);
    FileBody fileBody = new FileBody(file, mimetype);

    return MultipartEntityBuilder.create() //
        .setMode(HttpMultipartMode.EXTENDED) //
        .addPart("jar", fileBody) //
        .setCharset(StandardCharsets.UTF_8) //
        .setContentType(ContentType.MULTIPART_FORM_DATA).build();
  }

  /**
   * Obtain the content type, if possible, returning a default OCTET_STREAM if unable to detect it.
   *
   * @param file the file to test the content type.
   * @return the content type in a string, MimeTypes.OCTET_STREAM if unable to find it.
   */
  public String getContentType(File file) {
    return Try.of(() -> Files.probeContentType(file.toPath())) //
        .onFailure(throwable -> Try.of(() -> tika.detect(file))) //
        .map(s -> StringUtils.isNotBlank(s) ? s : MimeTypes.OCTET_STREAM)
        .getOrElse(MimeTypes.OCTET_STREAM); //
  }

}
