package com.webforj.devtools.livereload.receiver;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

/**
 * Stands in for the receiver the observer resolves by name in the running application, so the
 * observer tests can watch what arrives without the webforj-devtools jar.
 */
public final class HotswapAgentReceiver {

  private static final List<Object[]> reports = new CopyOnWriteArrayList<>();
  private static volatile CountDownLatch arrival = new CountDownLatch(1);

  private HotswapAgentReceiver() {}

  public static void onClassRedefinition(String className, byte[] classBytes) {
    reports.add(new Object[] {className, classBytes});
    arrival.countDown();
  }

  public static void reset() {
    reports.clear();
    arrival = new CountDownLatch(1);
  }

  public static List<Object[]> reports() {
    return reports;
  }

  public static boolean awaitReport(long timeout, TimeUnit unit) throws InterruptedException {
    return arrival.await(timeout, unit);
  }
}
