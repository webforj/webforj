package org.dwcj.component;

import org.dwcj.component.window.AbstractWindow;

public class AbstractDwcComponentMock extends AbstractDwcComponent
    implements HasExpanse<AbstractDwcComponentMock, Expanse> {

  @Override
  public AbstractDwcComponentMock setExpanse(Expanse expanse) {
    setComponentExpanse(expanse);
    return this;
  }

  @Override
  public Expanse getExpanse() {
    return (Expanse) getComponentExpanse();
  }

  @Override
  protected void create(AbstractWindow panel) {
    throw new UnsupportedOperationException("Unimplemented method 'create'");
  }
}
