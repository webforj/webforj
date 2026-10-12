package com.webforj.plugin.maven.bbj.model;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.File;
import org.junit.jupiter.api.Test;

class BBjOptionsTest {

  @Test
  void shouldStartWithDefaults() {
    BBjOptions options = new BBjOptions();

    assertNull(options.getHome());
    assertEquals("", options.getPublishName());
    assertNotNull(options.getAdmin());
    assertNull(options.getAdmin().getUsername());
    assertNotNull(options.getDeployment());
    assertEquals("", options.getDeployment().getUrl());
  }

  @Test
  void shouldHoldTheConfiguredValues() {
    BBjOptions options = new BBjOptions();
    File home = new File("bbx");
    BBjAdminOptions admin = new BBjAdminOptions();
    BBjDeploymentOptions deployment = new BBjDeploymentOptions();

    options.setHome(home);
    options.setPublishName("example");
    options.setAdmin(admin);
    options.setDeployment(deployment);

    assertSame(home, options.getHome());
    assertEquals("example", options.getPublishName());
    assertSame(admin, options.getAdmin());
    assertSame(deployment, options.getDeployment());
  }

  @Test
  void shouldRejectNullAndKeepTheConfiguredValues() {
    BBjOptions options = new BBjOptions();
    File home = new File("bbx");
    BBjAdminOptions admin = new BBjAdminOptions();
    BBjDeploymentOptions deployment = new BBjDeploymentOptions();
    options.setHome(home);
    options.setPublishName("example");
    options.setAdmin(admin);
    options.setDeployment(deployment);
    File missingHome = null;
    assertThrows(NullPointerException.class, () -> options.setHome(missingHome));
    String missingPublishName = null;
    assertThrows(NullPointerException.class, () -> options.setPublishName(missingPublishName));
    BBjAdminOptions missingAdmin = null;
    assertThrows(NullPointerException.class, () -> options.setAdmin(missingAdmin));
    BBjDeploymentOptions missingDeployment = null;
    assertThrows(NullPointerException.class, () -> options.setDeployment(missingDeployment));

    assertSame(home, options.getHome());
    assertEquals("example", options.getPublishName());
    assertSame(admin, options.getAdmin());
    assertSame(deployment, options.getDeployment());
  }

  @Test
  void shouldHoldTheDevelopmentConfiguration() {
    BBjOptions options = new BBjOptions();
    BBjDevelopmentOptions development = new BBjDevelopmentOptions();
    BBjDevelopmentOptions missing = null;

    assertNull(options.getDevelopment().getClassFolder());
    options.setDevelopment(development);
    assertThrows(NullPointerException.class, () -> options.setDevelopment(missing));

    assertSame(development, options.getDevelopment());
  }

  @Test
  void shouldHoldThePropertiesFile() {
    BBjOptions options = new BBjOptions();
    File properties = new File("bbx/cfg/BBj.properties");
    File missing = null;

    assertNull(options.getProperties());
    options.setProperties(properties);
    assertThrows(NullPointerException.class, () -> options.setProperties(missing));

    assertSame(properties, options.getProperties());
  }
}
