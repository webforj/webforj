package org.dwcj.component.optioninput;

import org.dwcj.component.window.AbstractWindow;

public class AbstractDwcOptionInputMock extends AbstractDwcOptionInput<AbstractDwcOptionInputMock> {

  public AbstractDwcOptionInputMock() {
    super("", false);
  }

  @Override
  protected void create(AbstractWindow panel) {
    throw new UnsupportedOperationException("Unimplemented method 'create'");
  }

}
