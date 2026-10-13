package com.webforj.bbjship.assets;

import com.google.common.net.HttpHeaders;
import com.webforj.bbjship.assets.model.AssetRequest;
import com.webforj.bbjship.assets.model.AssetResponse;
import com.webforj.plugin.foundation.bbj.BBjShipPlugin;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.nio.file.Path;

/**
 * Serves the static files and icons of published applications.
 *
 * <p>
 * The path below the assets context names the application in its first segment and the asset in the
 * rest. The servlet answers GET and HEAD only.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class AssetsServlet {

  private static final Logger LOGGER = System.getLogger(AssetsServlet.class.getName());
  private static final String GET = "GET";
  private static final String HEAD = "HEAD";
  private static final String NOT_FOUND = "The asset does not exist";

  private AssetsServlet() {}

  /**
   * Answers a GET or HEAD request for an asset of a published application.
   *
   * @param request the servlet request
   * @param response the servlet response
   *
   * @throws IOException if the answer cannot be written
   */
  public static void handle(HttpServletRequest request, HttpServletResponse response)
      throws IOException {
    BBjShipPlugin plugin = BBjShipPlugin.createFromBBjHome();
    handle(request, response, plugin.getDeployments());
  }

  static void handle(HttpServletRequest request, HttpServletResponse response, Path root)
      throws IOException {
    String method = request.getMethod();
    if (!GET.equals(method) && !HEAD.equals(method)) {
      response.sendError(HttpServletResponse.SC_METHOD_NOT_ALLOWED,
          "Only GET and HEAD requests are supported");
      return;
    }

    String path = request.getPathInfo() == null ? "" : request.getPathInfo();
    if (path.startsWith("/")) {
      path = path.substring(1);
    }

    int separator = path.indexOf('/');
    if (separator < 1 || separator == path.length() - 1) {
      response.sendError(HttpServletResponse.SC_NOT_FOUND, NOT_FOUND);
      return;
    }

    AssetRequest asset =
        AssetRequest.create(path.substring(0, separator), path.substring(separator + 1))
            .setPadding(request.getParameter("padding"))
            .setBackground(request.getParameter("background"))
            .setIfNoneMatch(request.getHeader(HttpHeaders.IF_NONE_MATCH)).build();
    AssetResponse answer;
    try {
      answer = DeployedAssets.read(root.toString(), asset);
    } catch (IOException | RuntimeException failure) {
      LOGGER.log(Level.ERROR, "webforJ BBj assets error", failure);
      response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
          "The asset could not be read. Check the BBjServices log");
      return;
    }

    writeAnswer(response, answer, HEAD.equals(method));
  }

  private static void writeAnswer(HttpServletResponse response, AssetResponse answer, boolean head)
      throws IOException {
    int status = answer.getStatus();
    if (status != HttpServletResponse.SC_OK && status != HttpServletResponse.SC_NOT_MODIFIED) {
      response.sendError(status, NOT_FOUND);
      return;
    }

    response.setStatus(status);
    if (answer.getEntityTag() != null) {
      response.setHeader(HttpHeaders.ETAG, answer.getEntityTag());
    }

    if (answer.getCacheControl() != null) {
      response.setHeader(HttpHeaders.CACHE_CONTROL, answer.getCacheControl());
    }

    if (status == HttpServletResponse.SC_NOT_MODIFIED) {
      return;
    }

    if (answer.getContentType() != null) {
      response.setContentType(answer.getContentType());
    }

    byte[] body = answer.getBody();
    response.setContentLength(body.length);
    if (!head) {
      response.getOutputStream().write(body);
    }
  }
}
