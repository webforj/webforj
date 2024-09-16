package com.webforj.component.layout.appnav;

import com.webforj.component.element.annotation.NodeName;

@NodeName("mock-navigation-container")
class MockNavigationContainer extends NavigationContainer<MockNavigationContainer> {

  private final String slotName;

  public MockNavigationContainer(String slotName) {
    this.slotName = slotName;
  }

  @Override
  protected String getSlotName() {
    return slotName;
  }
}
