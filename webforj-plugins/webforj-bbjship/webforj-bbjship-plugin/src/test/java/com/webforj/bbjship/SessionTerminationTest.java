package com.webforj.bbjship;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminBBjProcess;
import com.basis.api.admin.BBjAdminBase;
import java.io.IOException;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class SessionTerminationTest {

  private static final String CLASSPATH_NAME = "webforj_bbjship_hello-app";

  private BBjAdminBase admin;
  private BBjAdminBBjProcess owned;
  private BBjAdminBBjProcess other;

  @BeforeEach
  void initSessionsOfTwoClasspaths() throws Exception {
    admin = mock(BBjAdminBase.class);
    owned = mock(BBjAdminBBjProcess.class);
    other = mock(BBjAdminBBjProcess.class);
    when(owned.getString(BBjAdminBBjProcess.SSCP_NAME)).thenReturn(CLASSPATH_NAME);
    when(other.getString(BBjAdminBBjProcess.SSCP_NAME)).thenReturn("webforj_bbjship_other-app");
  }

  @Test
  void shouldTerminateOnlyTheSessionsOfTheClasspathAndEndOnceTheyAreGone() throws Exception {
    when(admin.getBBjProcesses()).thenReturn(List.of(owned, other), List.of(other));

    SessionTermination.stopProcesses(admin, CLASSPATH_NAME);

    verify(owned).terminate();
    verify(other, never()).terminate();
    verify(admin, times(2)).getBBjProcesses();
  }

  @Test
  void shouldFailAndKeepTheInterruptWhenTheWaitIsInterrupted() throws Exception {
    when(admin.getBBjProcesses()).thenReturn(List.of(owned));
    doAnswer(call -> {
      Thread.currentThread().interrupt();
      return null;
    }).when(owned).terminate();

    IOException failure = assertThrows(IOException.class,
        () -> SessionTermination.stopProcesses(admin, CLASSPATH_NAME));

    assertEquals(
        "Stopping the sessions of the BBj classpath webforj_bbjship_hello-app was interrupted",
        failure.getMessage());
    assertInstanceOf(InterruptedException.class, failure.getCause());
    assertTrue(Thread.interrupted());
  }
}
