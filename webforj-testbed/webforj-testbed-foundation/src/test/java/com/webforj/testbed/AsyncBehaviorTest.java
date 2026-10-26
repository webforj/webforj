package com.webforj.testbed;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.Environment;
import com.webforj.Interval;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;

class AsyncBehaviorTest extends TestbedTestSupport {

  @Test
  void shouldRunLaterSynchronouslyOnTheTestThread() {
    List<String> order = new ArrayList<>();

    order.add("before");
    Environment.runLater(() -> order.add("inside"));
    order.add("after");

    assertEquals(List.of("before", "inside", "after"), order);
  }

  @Test
  void shouldCompleteRunLaterResults() {
    List<String> results = new ArrayList<>();

    Environment.runLater(() -> "result").thenAccept(results::add);

    assertEquals(List.of("result"), results);
  }

  @Test
  void shouldFireIntervalsExplicitly() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    List<String> ticks = new ArrayList<>();
    Interval interval = new Interval(1f, event -> ticks.add("tick"));

    interval.start();

    assertEquals(1, context.runIntervals());
    assertEquals(1, context.runIntervals());

    assertEquals(2, ticks.size());

    interval.stop();

    assertEquals(0, context.runIntervals());
  }
}
