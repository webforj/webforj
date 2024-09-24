package com.webforj.router;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.stream.IntStream;
import org.junit.jupiter.api.Test;

class RoutePatternPerfTest {

  private static final int NUMBER_OF_PATHS = 1000;
  private static final String BASE_PATTERN = "/item/:id<[0-9]+>/details/:type";

  private RoutePattern createRoutePattern() {
    return new RoutePattern(BASE_PATTERN);
  }

  @Test
  void shouldMatchAgainstPaths() {
    RoutePattern pattern = createRoutePattern();
    Random random = new Random();

    // Generate 1000 paths with various parameter values
    Map<String, String> paths = new HashMap<>();
    IntStream.range(0, NUMBER_OF_PATHS).forEach(i -> {
      String id = String.valueOf(random.nextInt(1000));
      String type = random.nextBoolean() ? "A" : "B";
      String path = String.format("/item/%s/details/%s", id, type);
      paths.put(path, id + "," + type);
    });

    long startTime = System.nanoTime();

    // Perform matching
    paths.forEach((path, expectedParams) -> {
      assertTrue(pattern.matches(path), "Path should match: " + path);
      Map<String, String> params = pattern.getParameters(path);
      String[] expected = expectedParams.split(",");
      assertEquals(expected[0], params.get("id"), "Parameter 'id' should match");
      assertEquals(expected[1], params.get("type"), "Parameter 'type' should match");
    });

    long endTime = System.nanoTime();
    long duration = (endTime - startTime) / 1_000_000; // Convert to milliseconds
    assertTrue(duration < 500, "Performance test should complete in less than 500ms");
  }
}
