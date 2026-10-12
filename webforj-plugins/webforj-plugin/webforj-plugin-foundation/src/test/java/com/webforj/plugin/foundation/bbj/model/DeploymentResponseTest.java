package com.webforj.plugin.foundation.bbj.model;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.google.gson.Gson;
import org.junit.jupiter.api.Test;

class DeploymentResponseTest {

  @Test
  void shouldReadTheDeploymentAnswer() {
    String json = """
        {
          "name": "hello-app",
          "url": "/webapp/hello-app"
        }
        """;

    DeploymentResponse response = new Gson().fromJson(json, DeploymentResponse.class);

    assertEquals("hello-app", response.getName());
    assertEquals("/webapp/hello-app", response.getUrl());
  }
}
