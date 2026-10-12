package com.webforj.bbjship;

import static org.mockito.AdditionalAnswers.delegatesTo;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminAppDeploymentApplication;
import com.basis.api.admin.BBjAdminAppDeploymentConfiguration;
import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.rest.adapter.BBjAdminAdapterList;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.webforj.bbjship.BBjShipServlet.AdminSessionFactory;
import com.webforj.bbjship.cli.admin.BBjShipPlugin;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.ByteArrayOutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.io.TempDir;

/**
 * Holds the request of a BBj administrator to BBjShip on a server without applications.
 */
abstract class AdministratorRequest {

  /**
   * Formats JSON answers for comparison.
   */
  static final Gson JSON = new GsonBuilder().setPrettyPrinting().create();

  /**
   * Holds the temporary folder of the test.
   */
  @TempDir
  Path temporary;

  /**
   * Holds the BBjShip plugin of the BBj home.
   */
  BBjShipPlugin plugin;
  /**
   * Holds the BBjShip runtime folder.
   */
  Path runtime;
  /**
   * Holds the mocked request.
   */
  HttpServletRequest request;
  /**
   * Holds the mocked response.
   */
  HttpServletResponse response;
  /**
   * Holds the body written to the response.
   */
  ByteArrayOutputStream body;
  /**
   * Holds the mocked administration session factory.
   */
  AdminSessionFactory sessions;
  /**
   * Holds the mocked administration session.
   */
  BBjAdminBase admin;
  /**
   * Holds the mocked application deployment configuration.
   */
  BBjAdminAppDeploymentConfiguration configuration;

  /**
   * Creates the request of an administrator on a server without applications.
   */
  @BeforeEach
  void initRequestOfAdministratorOnServerWithoutApplications() throws Exception {
    plugin = new BBjShipPlugin(temporary);
    Files.createDirectories(temporary.resolve("plugins/webforj-bbjship"));
    runtime = Files.createDirectories(temporary.resolve("plugins/.webforj-bbjship-deployments"));
    request = mock(HttpServletRequest.class);
    response = mock(HttpServletResponse.class);
    body = new ByteArrayOutputStream();
    sessions = mock(AdminSessionFactory.class);
    admin = mock(BBjAdminBase.class);
    configuration = mock(BBjAdminAppDeploymentConfiguration.class);
    when(request.getHeader("Authorization")).thenReturn("Basic YWRtaW5pc3RyYXRvcjpzZWNyZXQ=");
    ServletOutputStream output = mock(ServletOutputStream.class, delegatesTo(body));
    when(response.getOutputStream()).thenReturn(output);
    when(sessions.create("administrator", "secret")).thenReturn(admin);
    when(admin.getRemoteConfiguration()).thenReturn(configuration);
    when(admin.getClasspathNames()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    when(configuration.getApplications()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    BBjAdminAppDeploymentApplication created = mock(BBjAdminAppDeploymentApplication.class);
    when(configuration.createApplication()).thenReturn(created);
  }
}
