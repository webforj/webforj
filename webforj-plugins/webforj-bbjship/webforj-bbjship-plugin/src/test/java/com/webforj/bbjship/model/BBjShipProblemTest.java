package com.webforj.bbjship.model;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.google.gson.GsonBuilder;
import org.junit.jupiter.api.Test;

final class BBjShipProblemTest {

  @Test
  void shouldWriteTheProblemDetailsMembers() {
    BBjShipProblem problem = BBjShipProblem.create(409).setType("about:blank").setTitle("Conflict")
        .setDetail("Choose another publish name").build();

    assertEquals("about:blank", problem.getType());
    assertEquals("Conflict", problem.getTitle());
    assertEquals(409, problem.getStatus());
    assertEquals("Choose another publish name", problem.getDetail());
    assertEquals("""
        {
          "type": "about:blank",
          "title": "Conflict",
          "status": 409,
          "detail": "Choose another publish name"
        }""", new GsonBuilder().setPrettyPrinting().create().toJson(problem));
  }
}
