package com.webforj.bbjship.model;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.google.gson.GsonBuilder;
import org.junit.jupiter.api.Test;

final class BBjShipDeploymentTest {

  @Test
  void shouldWriteTheNameAndUrlFields() {
    BBjShipDeployment deployment = new BBjShipDeployment("hello-app", "/webapp/hello-app");

    assertEquals("hello-app", deployment.getName());
    assertEquals("/webapp/hello-app", deployment.getUrl());
    assertEquals("""
        {
          "name": "hello-app",
          "url": "/webapp/hello-app"
        }""", new GsonBuilder().setPrettyPrinting().create().toJson(deployment));
  }
}
