package com.webforj.data.binding;

class ReadCountingNameComponentMock extends NameComponentMock {
  private int readCount = 0;

  @Override
  public String getValue() {
    readCount++;
    return super.getValue();
  }

  int getReadCount() {
    return readCount;
  }
}
