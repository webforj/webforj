package org.dwcj.component;

import org.dwcj.component.window.AbstractWindow;

public class AbstractOptionInputMock extends AbstractOptionInput<AbstractOptionInputMock> {

  public AbstractOptionInputMock() {
    super("", false);
  }

  @Override
  protected void create(AbstractWindow panel) {
    throw new UnsupportedOperationException("Unimplemented method 'create'");
  }

}
