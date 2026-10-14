package com.webforj.bbjship;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminAppDeploymentApplication;
import com.basis.api.admin.BBjAdminAppDeploymentConfiguration;
import com.basis.api.admin.rest.adapter.BBjAdminAdapterList;
import com.webforj.bbjship.foundation.HelloApp;
import com.webforj.bbjship.model.DeploymentRegistration;
import com.webforj.bbjship.model.InstalledDeployment;
import java.io.ByteArrayInputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class RegistrationMappingTest {

  private static final String APP_NAME = "hello-app";
  private static final String FIRST_ARGUMENT = "first";

  @TempDir
  Path temporary;

  private InstalledDeployment deployment;
  private BBjAdminAppDeploymentConfiguration configuration;
  private BBjAdminAppDeploymentApplication application;

  @BeforeEach
  void initHelloAppDeployment() throws Exception {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Files.writeString(app.resolve("src/main/resources/webforj.conf"), """
        webforj {
          programArguments = ["first", "second"]
        }
        """);
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    deployment =
        DeploymentStaging.writeDeployment(new ByteArrayInputStream(Files.readAllBytes(archive)),
            temporary.resolve("deployments"), APP_NAME, List.of());
    configuration = mock(BBjAdminAppDeploymentConfiguration.class);
    application = mock(BBjAdminAppDeploymentApplication.class);
    when(configuration.createApplication()).thenReturn(application);
    when(application.getArguments()).thenReturn(new BBjAdminAdapterList<>(new ArrayList<>()));
  }

  @Test
  void shouldMapTheDeploymentToAnApplicationWithoutCommittingIt() throws Exception {
    BBjAdminAppDeploymentApplication mapped = RegistrationMapping.createApplication(configuration,
        deployment, APP_NAME, "webforj_bbjship_hello-app");

    Path program = deployment.getDirectory().resolve("bbj/webforj.bbj");
    assertSame(application, mapped);
    verify(application).setString(BBjAdminAppDeploymentApplication.NAME, APP_NAME);
    verify(application).setString(BBjAdminAppDeploymentApplication.PROGRAM, program.toString());
    verify(application).setString(BBjAdminAppDeploymentApplication.CLASSPATH,
        "webforj_bbjship_hello-app");
    verify(application).setString(BBjAdminAppDeploymentApplication.WORKING_DIRECTORY,
        program.getParent().toString());
    verify(application).setBoolean(BBjAdminAppDeploymentApplication.DWC_ENABLED, true);
    assertEquals(List.of(FIRST_ARGUMENT, "second"), application.getArguments());
    verify(application, never()).commit();
  }

  @Test
  void shouldMapTheClasspathAfterTheBbjShipClasspath() {
    List<String> classpath = RegistrationMapping.createClasspath(deployment);

    List<String> expected = new ArrayList<>(List.of("(_webforj_bbjship)"));
    deployment.getClasspath().forEach(path -> expected.add(path.toString()));
    assertEquals(expected, classpath);
  }

  @Test
  void shouldKeepThePropertiesAndArgumentsOfTheRegistration() throws Exception {
    when(application.getProperties()).thenReturn(Map.of("NAME", APP_NAME));
    when(application.getJavaArguments()).thenReturn(new BBjAdminAdapterList<>(List.of("-Xmx512m")));
    application.getArguments().add(FIRST_ARGUMENT);

    DeploymentRegistration registration = RegistrationMapping.createRegistration(application);

    assertEquals(Map.of("NAME", APP_NAME), registration.getProperties());
    assertEquals(List.of(FIRST_ARGUMENT), registration.getArguments());
    assertEquals(List.of("-Xmx512m"), registration.getJavaArguments());
  }
}
