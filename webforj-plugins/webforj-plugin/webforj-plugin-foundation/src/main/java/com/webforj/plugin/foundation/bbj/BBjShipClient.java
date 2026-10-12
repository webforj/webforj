package com.webforj.plugin.foundation.bbj;

import com.google.gson.Gson;
import com.google.gson.JsonParseException;
import com.webforj.plugin.foundation.bbj.model.BBjCredentials;
import com.webforj.plugin.foundation.bbj.model.DeploymentResponse;
import com.webforj.plugin.foundation.bbj.model.ProblemResponse;
import com.webforj.plugin.foundation.bbj.model.VersionResponse;
import java.io.File;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Locale;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Consumer;
import javax.net.ssl.SSLException;
import org.apache.hc.client5.http.auth.AuthScheme;
import org.apache.hc.client5.http.auth.AuthenticationException;
import org.apache.hc.client5.http.auth.BearerToken;
import org.apache.hc.client5.http.auth.UsernamePasswordCredentials;
import org.apache.hc.client5.http.classic.methods.HttpDelete;
import org.apache.hc.client5.http.classic.methods.HttpGet;
import org.apache.hc.client5.http.classic.methods.HttpPut;
import org.apache.hc.client5.http.config.ConnectionConfig;
import org.apache.hc.client5.http.config.RequestConfig;
import org.apache.hc.client5.http.impl.auth.BasicScheme;
import org.apache.hc.client5.http.impl.auth.BearerScheme;
import org.apache.hc.client5.http.impl.auth.CredentialsProviderBuilder;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.client5.http.impl.classic.HttpClients;
import org.apache.hc.client5.http.impl.io.PoolingHttpClientConnectionManagerBuilder;
import org.apache.hc.client5.http.protocol.HttpClientContext;
import org.apache.hc.core5.http.ClassicHttpRequest;
import org.apache.hc.core5.http.ClassicHttpResponse;
import org.apache.hc.core5.http.ContentType;
import org.apache.hc.core5.http.HttpEntity;
import org.apache.hc.core5.http.HttpHost;
import org.apache.hc.core5.http.HttpStatus;
import org.apache.hc.core5.http.io.HttpClientResponseHandler;
import org.apache.hc.core5.http.io.entity.FileEntity;
import org.apache.hc.core5.net.URIBuilder;
import org.apache.hc.core5.util.Timeout;

/**
 * Sends application deployment requests to BBjShip of a BBj installation.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class BBjShipClient {

  private static final int MAX_RESPONSE_BYTES = 65536;
  private static final Timeout CONNECT_TIMEOUT = Timeout.ofSeconds(30);
  private static final Timeout RESPONSE_TIMEOUT = Timeout.ofMinutes(5);
  private static final ContentType ARCHIVE = ContentType.create("application/zip");
  private static final ContentType PROBLEM = ContentType.create("application/problem+json");
  private static final String HTTP = "http";
  private static final String DEPLOYMENT = "BBj deployment";
  private static final String UNDEPLOYMENT = "BBj undeployment";
  private static final String VERSION_CHECK = "BBjShip check";
  private static final String SETUP_HINT = "webforj:bbj-setup on the BBj host";
  private static final String VERSION_FAILURE = "could not read the BBjShip version at ";
  private static final String UNCONFIRMED =
      "did not confirm the requested application. Check the BBjShip version and BBjServices log";
  private static final String UPLOAD_PROGRESS =
      "Uploading " + DEPLOYMENT + ", %d of %d bytes (%d%%)";

  private final URI endpoint;
  private final BBjCredentials credentials;
  private final String version;
  private final Consumer<String> log;
  private final Consumer<String> warn;
  private final Gson gson = new Gson();

  private BBjShipClient(Builder builder) {
    this.endpoint = builder.endpoint;
    this.credentials = builder.credentials;
    this.version = builder.version;
    this.log = builder.log;
    this.warn = builder.warn;
  }

  /**
   * Creates a new builder for a client of BBjShip.
   *
   * @param endpoint the HTTP or HTTPS base address of BBjShip
   *
   * @return a new builder
   * @throws NullPointerException if endpoint is null
   * @throws IllegalArgumentException if the endpoint is not an HTTP or HTTPS URL with a host, or
   *         carries user information, a query or a fragment
   */
  public static Builder create(URI endpoint) {
    String scheme = Objects.requireNonNull(endpoint).getScheme();
    boolean web = HTTP.equalsIgnoreCase(scheme) || "https".equalsIgnoreCase(scheme);
    if (!web || endpoint.getHost() == null || endpoint.getUserInfo() != null
        || endpoint.getQuery() != null || endpoint.getFragment() != null) {
      throw new IllegalArgumentException("BBj deployment URL must be an HTTP or HTTPS URL "
          + "without credentials, query or fragment");
    }

    return new Builder(endpoint);
  }

  /**
   * Uploads an archive and verifies publication of the requested application.
   *
   * @param archive the deployment archive
   * @param publishName the name the application is published under
   *
   * @return the address of the published application
   * @throws NullPointerException if an argument is null
   * @throws IOException if the archive is missing, the BBjShip version differs, BBjShip refuses the
   *         request or publication is not confirmed
   */
  public URI deploy(Path archive, String publishName) throws IOException {
    Objects.requireNonNull(archive);
    Objects.requireNonNull(publishName);
    getPlainTextWarning().ifPresent(warn);
    try {
      if (!Files.isRegularFile(archive)) {
        throw new BBjShipRequestException("archive is missing. Package the application first");
      }

      HttpPut request = new HttpPut(resolve("applications", publishName));
      request.setEntity(new UploadEntity(archive.toFile(), log));
      DeploymentResponse deployment =
          send(request, response -> readJson(response, DeploymentResponse.class));
      if (deployment == null || !publishName.equals(deployment.getName())
          || deployment.getUrl() == null || deployment.getUrl().isBlank()) {
        throw new BBjShipRequestException(UNCONFIRMED);
      }

      return endpoint.resolve(deployment.getUrl());
    } catch (BBjShipRequestException failure) {
      throw new IOException(DEPLOYMENT + " " + failure.getMessage(), failure.getCause());
    }
  }

  /**
   * Removes a published application.
   *
   * @param publishName the name the application is published under
   *
   * @throws NullPointerException if publishName is null
   * @throws IOException if the BBjShip version differs or BBjShip refuses the request
   */
  public void undeploy(String publishName) throws IOException {
    Objects.requireNonNull(publishName);
    getPlainTextWarning().ifPresent(warn);
    try {
      send(new HttpDelete(resolve("applications", publishName)), response -> {
        readSuccess(response);
        return publishName;
      });
    } catch (BBjShipRequestException failure) {
      throw new IOException(UNDEPLOYMENT + " " + failure.getMessage(), failure.getCause());
    }
  }

  /**
   * Reads the version and the Java runtime BBjShip answers with.
   *
   * @return the version answer of BBjShip
   * @throws IOException if BBjShip refuses the request or does not answer with a version
   */
  public VersionResponse getVersion() throws IOException {
    try {
      return send((client, context) -> fetchVersion(client, context, this::readVersionResponse));
    } catch (BBjShipRequestException failure) {
      throw new IOException(VERSION_CHECK + " " + failure.getMessage(), failure.getCause());
    }
  }

  private Optional<String> getPlainTextWarning() {
    String host = endpoint.getHost().toLowerCase(Locale.ROOT);
    boolean loopback = host.equals("localhost") || host.startsWith("127.") || host.equals("[::1]")
        || host.equals("::1");
    if (!HTTP.equalsIgnoreCase(endpoint.getScheme()) || loopback) {
      return Optional.empty();
    }

    return Optional.of(DEPLOYMENT + " URL " + endpoint
        + " sends the administration credentials readable over the network. "
        + "Use an https:// URL for a host that is not this machine");
  }

  private <T> T send(ClassicHttpRequest request, HttpClientResponseHandler<T> handler)
      throws IOException {
    return send((client, context) -> {
      String installed = fetchVersion(client, context, this::readVersion);
      if (!version.equals(installed)) {
        throw new IOException("BBjShip " + installed + " at " + endpoint
            + " does not match webforJ " + version + ". Run " + SETUP_HINT);
      }

      return client.execute(request, context, handler);
    });
  }

  private <T> T send(BBjShipExchange<T> exchange) throws IOException {
    HttpHost target = HttpHost.create(endpoint);
    HttpClientContext context = HttpClientContext.create();
    context.resetAuthExchange(target, createAuthentication(target, context));
    try (CloseableHttpClient client = createClient()) {
      return exchange.send(client, context);
    } catch (SSLException failure) {
      throw createTrustFailure(failure);
    }
  }

  private <T> T fetchVersion(CloseableHttpClient client, HttpClientContext context,
      HttpClientResponseHandler<T> handler) throws IOException {
    return client.execute(new HttpGet(resolve("version")), context, handler);
  }

  private AuthScheme createAuthentication(HttpHost target, HttpClientContext context)
      throws BBjShipRequestException {
    if (!credentials.isToken()) {
      BasicScheme basic = new BasicScheme();
      basic.initPreemptive(new UsernamePasswordCredentials(credentials.getUsername(),
          credentials.getPassword().toCharArray()));

      return basic;
    }

    BearerScheme bearer = new BearerScheme();
    try {
      bearer.isResponseReady(target, CredentialsProviderBuilder.create()
          .add(target, new BearerToken(credentials.getToken())).build(), context);
    } catch (AuthenticationException failure) {
      throw new BBjShipRequestException("could not prepare the authorization token", failure);
    }

    return bearer;
  }

  private URI resolve(String... segments) throws BBjShipRequestException {
    try {
      return new URIBuilder(endpoint).appendPathSegments(segments).build();
    } catch (URISyntaxException failure) {
      throw new BBjShipRequestException("could not build a request URL from " + endpoint, failure);
    }
  }

  private BBjShipRequestException createTrustFailure(SSLException failure) {
    String detail = Objects.toString(failure.getMessage(), "").strip();
    return new BBjShipRequestException("could not establish a trusted HTTPS connection to "
        + endpoint + ". Install a certificate for the host name of the URL on the BBj host or "
        + "add its certificate to the Java truststore of the build."
        + (detail.isEmpty() ? "" : " " + detail), failure);
  }

  private String readVersion(ClassicHttpResponse response) throws IOException {
    return readVersionResponse(response).getVersion();
  }

  private VersionResponse readVersionResponse(ClassicHttpResponse response) throws IOException {
    int status = response.getCode();
    if (status != HttpStatus.SC_UNAUTHORIZED && !isSuccess(status)) {
      throw new BBjShipRequestException(VERSION_FAILURE + endpoint + ", HTTP " + status
          + ". Check the deployment URL and run " + SETUP_HINT);
    }

    VersionResponse installed = readJson(response, VersionResponse.class);
    if (installed == null || installed.getVersion() == null || installed.getVersion().isBlank()) {
      throw new BBjShipRequestException(VERSION_FAILURE + endpoint + ". Run " + SETUP_HINT);
    }

    return installed;
  }

  private void readSuccess(ClassicHttpResponse response) throws IOException {
    int status = response.getCode();
    if (isSuccess(status)) {
      return;
    }

    String detail = "Check the endpoint, administration credentials and BBjServices log";
    HttpEntity entity = response.getEntity();
    if (entity != null && entity.getContentType() != null
        && ContentType.parse(entity.getContentType()).isSameMimeType(PROBLEM)) {
      ProblemResponse problem = readBody(entity, ProblemResponse.class);
      if (problem != null && problem.getDetail() != null && !problem.getDetail().isBlank()) {
        detail = problem.getDetail();
      }
    }

    throw new BBjShipRequestException("failed with HTTP " + status + ". " + detail);
  }

  private <T> T readJson(ClassicHttpResponse response, Class<T> type) throws IOException {
    readSuccess(response);
    HttpEntity entity = response.getEntity();

    return entity == null ? null : readBody(entity, type);
  }

  private <T> T readBody(HttpEntity entity, Class<T> type) throws IOException {
    try (InputStream input = entity.getContent()) {
      byte[] content = input.readNBytes(MAX_RESPONSE_BYTES + 1);
      if (content.length > MAX_RESPONSE_BYTES) {
        throw new BBjShipRequestException("response exceeds the allowed size");
      }

      return gson.fromJson(new String(content, StandardCharsets.UTF_8), type);
    } catch (JsonParseException invalid) {
      return null;
    }
  }

  private static CloseableHttpClient createClient() {
    ConnectionConfig connection = ConnectionConfig.custom().setConnectTimeout(CONNECT_TIMEOUT)
        .setSocketTimeout(RESPONSE_TIMEOUT).build();
    RequestConfig configuration =
        RequestConfig.custom().setConnectionRequestTimeout(CONNECT_TIMEOUT)
            .setResponseTimeout(RESPONSE_TIMEOUT).setRedirectsEnabled(false).build();

    return HttpClients.custom()
        .setConnectionManager(PoolingHttpClientConnectionManagerBuilder.create()
            .setDefaultConnectionConfig(connection).build())
        .setDefaultRequestConfig(configuration).disableAutomaticRetries().build();
  }

  private static boolean isSuccess(int status) {
    return status >= HttpStatus.SC_SUCCESS && status < HttpStatus.SC_REDIRECTION;
  }

  /**
   * Builds a {@link BBjShipClient}.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  public static final class Builder {

    private final URI endpoint;
    private BBjCredentials credentials;
    private String version = "";
    private Consumer<String> log = line -> {
    };
    private Consumer<String> warn = line -> {
    };

    private Builder(URI endpoint) {
      this.endpoint = endpoint;
    }

    /**
     * Sets the administration credentials sent with every request.
     *
     * @param credentials the credentials
     *
     * @return this builder
     * @throws NullPointerException if credentials is null
     */
    public Builder setCredentials(BBjCredentials credentials) {
      this.credentials = Objects.requireNonNull(credentials);
      return this;
    }

    /**
     * Sets the webforJ version BBjShip must match.
     *
     * @param version the webforJ version of the build
     *
     * @return this builder
     * @throws NullPointerException if version is null
     */
    public Builder setVersion(String version) {
      this.version = Objects.requireNonNull(version);
      return this;
    }

    /**
     * Sets where deployment upload progress is reported.
     *
     * @param log the log sink
     *
     * @return this builder
     * @throws NullPointerException if log is null
     */
    public Builder setLog(Consumer<String> log) {
      this.log = Objects.requireNonNull(log);
      return this;
    }

    /**
     * Sets where warnings are reported.
     *
     * @param warn the warning sink
     *
     * @return this builder
     * @throws NullPointerException if warn is null
     */
    public Builder setWarn(Consumer<String> warn) {
      this.warn = Objects.requireNonNull(warn);
      return this;
    }

    /**
     * Builds the client.
     *
     * @return the client
     */
    public BBjShipClient build() {
      return new BBjShipClient(this);
    }
  }

  /**
   * Reports a failed BBjShip request with a message that follows the name of the operation.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  private static final class BBjShipRequestException extends IOException {

    private static final long serialVersionUID = 1L;

    private BBjShipRequestException(String detail) {
      super(detail);
    }

    private BBjShipRequestException(String detail, Throwable cause) {
      super(detail, cause);
    }
  }

  /**
   * Reports deployment archive upload progress.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  private static final class UploadEntity extends FileEntity {

    private final Consumer<String> log;

    private UploadEntity(File file, Consumer<String> log) {
      super(file, ARCHIVE);
      this.log = log;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void writeTo(OutputStream output) throws IOException {
      long total = getContentLength();
      log.accept(UPLOAD_PROGRESS.formatted(0, total, 0));
      super.writeTo(new FilterOutputStream(output) {
        private long sent;
        private int reportedPercentage;
        private long reportedTime = System.nanoTime();

        /**
         * {@inheritDoc}
         */
        @Override
        public void write(byte[] bytes, int offset, int length) throws IOException {
          out.write(bytes, offset, length);
          handleProgress(length);
        }

        private void handleProgress(int length) {
          sent += length;
          int percentage = (int) (sent * 100.0 / total);
          long now = System.nanoTime();
          if (percentage < 100
              && (percentage >= reportedPercentage + 10 || now - reportedTime >= 1_000_000_000L)) {
            log.accept(UPLOAD_PROGRESS.formatted(sent, total, percentage));
            reportedPercentage = percentage;
            reportedTime = now;
          }
        }
      });
      log.accept(UPLOAD_PROGRESS.formatted(total, total, 100));
      log.accept("Upload complete, waiting for " + DEPLOYMENT + " confirmation");
    }
  }

  /**
   * Sends requests of one exchange with BBjShip through one client and one authentication.
   *
   * @param <T> the type of the answer
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  @FunctionalInterface
  private interface BBjShipExchange<T> {

    /**
     * Sends the requests of the exchange.
     *
     * @param client the client
     * @param context the context carrying the authentication
     *
     * @return the answer
     * @throws IOException if a request fails
     */
    T send(CloseableHttpClient client, HttpClientContext context) throws IOException;
  }
}
