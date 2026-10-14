package com.webforj.bbjship;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminException;
import com.google.common.net.HttpHeaders;
import com.google.gson.Gson;
import com.webforj.bbjship.Deployer.DeploymentResult;
import com.webforj.bbjship.foundation.BBjShipPlugin;
import com.webforj.bbjship.foundation.PublishNamePolicy;
import com.webforj.bbjship.foundation.model.BBjCredentials;
import com.webforj.bbjship.model.BBjShipDeployment;
import com.webforj.bbjship.model.BBjShipProblem;
import com.webforj.bbjship.model.BBjShipVersion;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.lang.management.ManagementFactory;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Optional;

/**
 * Answers the BBjShip HTTP API for the version, and the deployment and removal of applications.
 *
 * <p>
 * The API answers {@code GET /version}, {@code PUT /applications/{name}} and
 * {@code DELETE /applications/{name}} below the BBjShip context. Every request carries the Basic
 * credentials or the Bearer authorization token of a BBj administrator. Successful answers carry a
 * JSON object, refusals carry problem details.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class BBjShipServlet {

  /**
   * Names the problems BBjShip answers, with their status code and standard reason phrase.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  private enum Problem {
    // @formatter:off
    BAD_REQUEST(HttpServletResponse.SC_BAD_REQUEST, "Bad Request"),
    UNAUTHORIZED(HttpServletResponse.SC_UNAUTHORIZED, "Unauthorized"),
    FORBIDDEN(HttpServletResponse.SC_FORBIDDEN, "Forbidden"),
    NOT_FOUND(HttpServletResponse.SC_NOT_FOUND, "Not Found"),
    METHOD_NOT_ALLOWED(HttpServletResponse.SC_METHOD_NOT_ALLOWED, "Method Not Allowed"),
    CONFLICT(HttpServletResponse.SC_CONFLICT, "Conflict"),
    CONTENT_TOO_LARGE(HttpServletResponse.SC_REQUEST_ENTITY_TOO_LARGE, "Content Too Large"),
    INTERNAL_SERVER_ERROR(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, "Internal Server Error");
    // @formatter:on

    private final int status;
    private final String title;

    Problem(int status, String title) {
      this.status = status;
      this.title = title;
    }
  }

  private static final Logger LOGGER = System.getLogger(BBjShipServlet.class.getName());
  private static final Gson GSON = new Gson();
  private static final String VERSION_PATH = "/version";
  private static final String APPLICATIONS_PATH = "/applications/"; // NOSONAR
  private static final String GET = "GET";
  private static final String PUT = "PUT";
  private static final String DELETE = "DELETE";
  private static final String JSON = "application/json";
  private static final String PROBLEM_JSON = "application/problem+json";
  private static final String BASIC = "Basic";
  private static final String BEARER = "Bearer";
  private static final String REALM = " realm=\"webforj-bbjship\"";

  /**
   * The Basic challenge BBjShip answers a request without valid credentials with.
   */
  public static final String BASIC_CHALLENGE = BASIC + REALM;

  private BBjShipServlet() {}

  /**
   * Answers one request of the BBjShip HTTP API.
   *
   * @param request the servlet request
   * @param response the servlet response
   *
   * @throws IOException if the answer cannot be written
   */
  public static void handle(HttpServletRequest request, HttpServletResponse response)
      throws IOException {
    BBjShipPlugin plugin = BBjShipPlugin.createFromBBjHome();
    handle(request, response, plugin, new BBjServicesClasspathRelease(),
        new BBjServicesAdminSessionFactory());
  }

  static void handle(HttpServletRequest request, HttpServletResponse response, BBjShipPlugin plugin,
      ClasspathRelease release, AdminSessionFactory sessions) throws IOException {
    Optional<BBjAdminBase> session = readCredentials(request.getHeader(HttpHeaders.AUTHORIZATION))
        .flatMap(credentials -> openSession(credentials, sessions));
    if (session.isEmpty()) {
      response.setHeader(HttpHeaders.WWW_AUTHENTICATE, BASIC_CHALLENGE + ", " + BEARER + REALM);
      writeProblem(response, Problem.UNAUTHORIZED,
          "Check the BBj administration credentials or token");
      return;
    }

    BBjAdminBase admin = session.get();
    try {
      handleOperation(request, response, plugin, release, admin);
    } catch (DeploymentException refused) {
      writeProblem(response, toProblem(refused), refused.getMessage());
    } catch (IOException | RuntimeException failure) {
      LOGGER.log(Level.ERROR, "webforJ BBj deployment error", failure);
      writeProblem(response, Problem.INTERNAL_SERVER_ERROR,
          "BBj deployment failed. Check the BBjServices log");
    } finally {
      admin.release();
    }
  }

  private static void handleOperation(HttpServletRequest request, HttpServletResponse response,
      BBjShipPlugin plugin, ClasspathRelease release, BBjAdminBase admin) throws IOException {
    String method = request.getMethod();
    String path = request.getPathInfo();
    if (VERSION_PATH.equals(path)) {
      if (!GET.equals(method)) {
        writeMethodProblem(response, method, GET);
        return;
      }

      writeBody(response, HttpServletResponse.SC_OK, JSON, new BBjShipVersion(plugin.readVersion(),
          ManagementFactory.getRuntimeMXBean().getInputArguments()));
      return;
    }

    Optional<String> name = findApplicationName(path);
    if (name.isEmpty()) {
      writeProblem(response, Problem.NOT_FOUND,
          "BBjShip has no resource at " + path + ". Check the deployment URL");
      return;
    }

    if (!PUT.equals(method) && !DELETE.equals(method)) {
      writeMethodProblem(response, method, PUT + ", " + DELETE);
      return;
    }

    String publishName = name.get();
    Optional<String> refusal = PublishNamePolicy.findRefusal(publishName);
    if (refusal.isPresent()) {
      writeProblem(response, Problem.BAD_REQUEST, refusal.get());
      return;
    }

    String root = plugin.getDeployments().toString();
    if (DELETE.equals(method)) {
      Deployer.undeploy(publishName, root, admin, release);
      response.setStatus(HttpServletResponse.SC_NO_CONTENT);
      return;
    }

    DeploymentResult result =
        Deployer.deploy(request.getInputStream(), publishName, root, admin, release);
    String url = Deployer.getApplicationUrl(publishName);
    response.setHeader(HttpHeaders.LOCATION, url);
    int status = result == DeploymentResult.CREATED ? HttpServletResponse.SC_CREATED
        : HttpServletResponse.SC_OK;
    writeBody(response, status, JSON, new BBjShipDeployment(publishName, url));
  }

  private static Optional<BBjCredentials> readCredentials(String authorization) {
    String value = authorization == null ? "" : authorization.strip();
    int separator = value.indexOf(' ');
    if (separator < 0) {
      return Optional.empty();
    }

    String scheme = value.substring(0, separator);
    String parameter = value.substring(separator + 1).strip();
    if (BEARER.equalsIgnoreCase(scheme)) {
      return Optional.of(BBjCredentials.createToken(parameter));
    }

    if (!BASIC.equalsIgnoreCase(scheme)) {
      return Optional.empty();
    }

    String userPass;
    try {
      userPass = new String(Base64.getDecoder().decode(parameter), StandardCharsets.UTF_8);
    } catch (IllegalArgumentException invalid) {
      return Optional.empty();
    }

    int colon = userPass.indexOf(':');
    if (colon < 0) {
      return Optional.empty();
    }

    return Optional
        .of(BBjCredentials.createUser(userPass.substring(0, colon), userPass.substring(colon + 1)));
  }

  private static Optional<BBjAdminBase> openSession(BBjCredentials credentials,
      AdminSessionFactory sessions) {
    try {
      return Optional.of(sessions.create(credentials));
    } catch (BBjAdminException rejected) {
      return Optional.empty();
    }
  }

  private static Optional<String> findApplicationName(String path) {
    if (path == null || !path.startsWith(APPLICATIONS_PATH)) {
      return Optional.empty();
    }

    String name = path.substring(APPLICATIONS_PATH.length());
    if (name.isEmpty() || name.indexOf('/') >= 0) {
      return Optional.empty();
    }

    return Optional.of(name);
  }

  private static Problem toProblem(DeploymentException refused) {
    return switch (refused.getReason()) {
      case INVALID_NAME, INVALID_ARCHIVE -> Problem.BAD_REQUEST;
      case ARCHIVE_TOO_LARGE -> Problem.CONTENT_TOO_LARGE;
      case NAME_CONFLICT, ENTRY_CONFLICT, MODE_CONFLICT -> Problem.CONFLICT;
      case NOT_FOUND -> Problem.NOT_FOUND;
      case DEVELOPMENT_DISABLED -> Problem.FORBIDDEN;
    };
  }

  private static void writeMethodProblem(HttpServletResponse response, String method, String allow)
      throws IOException {
    response.setHeader(HttpHeaders.ALLOW, allow);
    writeProblem(response, Problem.METHOD_NOT_ALLOWED,
        "Method " + method + " does not apply to this resource. Use " + allow);
  }

  private static void writeProblem(HttpServletResponse response, Problem problem, String detail)
      throws IOException {
    BBjShipProblem body = BBjShipProblem.create(problem.status).setType("about:blank")
        .setTitle(problem.title).setDetail(detail).build();
    writeBody(response, problem.status, PROBLEM_JSON, body);
  }

  private static void writeBody(HttpServletResponse response, int status, String contentType,
      Object body) throws IOException {
    byte[] json = GSON.toJson(body).getBytes(StandardCharsets.UTF_8);
    response.setStatus(status);
    response.setContentType(contentType);
    response.setContentLength(json.length);
    response.getOutputStream().write(json);
  }
}
