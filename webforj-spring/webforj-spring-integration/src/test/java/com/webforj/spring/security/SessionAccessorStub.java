package com.webforj.spring.security;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;

import jakarta.servlet.http.HttpSession;
import java.util.function.Consumer;

final class SessionAccessorStub {

  private SessionAccessorStub() {}

  static HttpSession.Accessor create(HttpSession session) {
    HttpSession.Accessor accessor = mock(HttpSession.Accessor.class);
    doAnswer(invocation -> {
      Consumer<HttpSession> consumer = invocation.getArgument(0);
      consumer.accept(session);
      return null;
    }).when(accessor).access(any());
    return accessor;
  }
}
