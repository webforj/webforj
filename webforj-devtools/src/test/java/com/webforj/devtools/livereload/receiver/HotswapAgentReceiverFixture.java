package com.webforj.devtools.livereload.receiver;

import com.webforj.devtools.livereload.LiveReloadServer;
import java.util.List;

/**
 * Builds receivers with controlled virtual machine arguments for tests outside this package.
 */
public final class HotswapAgentReceiverFixture {

  private HotswapAgentReceiverFixture() {}

  /**
   * Creates a receiver that believes the agent is attached.
   *
   * @param server the reload server the update is pushed through
   * @return the receiver
   */
  public static HotswapAgentReceiver withAgentDetected(LiveReloadServer server) {
    return new HotswapAgentReceiver(server,
        List.of("-javaagent:/tools/hotswap-agent-2.0.3.jar=autoHotswap=true"));
  }
}
