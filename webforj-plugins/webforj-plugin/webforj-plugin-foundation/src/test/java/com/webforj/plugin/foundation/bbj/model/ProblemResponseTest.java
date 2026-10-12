package com.webforj.plugin.foundation.bbj.model;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.google.gson.Gson;
import org.junit.jupiter.api.Test;

class ProblemResponseTest {

  @Test
  void shouldReadTheDetailOfProblemDetails() {
    String json = """
        {
          "type": "about:blank",
          "title": "Conflict",
          "status": 409,
          "detail": "Choose another publish name"
        }
        """;

    ProblemResponse response = new Gson().fromJson(json, ProblemResponse.class);

    assertEquals("Choose another publish name", response.getDetail());
  }
}
