package com.webforj.component;

import com.basis.startup.type.BBjException;
import java.util.ArrayList;
import java.util.List;

class JsExecutorMock extends JsExecutor {

  private int asyncScriptIdCounter = 0;
  private List<String> executedScripts = new ArrayList<>();

  protected JsExecutorMock(Component component) {
    super(component);
  }

  @Override
  protected Object doExecuteJs(String script) throws BBjException {
    executedScripts.add(script);
    return "Executed: " + script;
  }

  @Override
  protected int doExecuteJsAsync(String script) throws BBjException {
    executedScripts.add(script);
    return asyncScriptIdCounter++;
  }

  public boolean isExecuted(String script) {
    return executedScripts.contains(script);
  }
}
