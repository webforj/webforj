package com.webforj.bbjship.foundation;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.UnknownHostException;
import java.util.Collections;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class LocalMachineTest {

  @ParameterizedTest
  @ValueSource(strings = {"localhost", "127.0.0.1", "127.0.0.2", "::1", "[::1]",
      "[0:0:0:0:0:0:0:1]", "::ffff:127.0.0.1"})
  void shouldRecogniseLoopbackHosts(String host) throws UnknownHostException {
    assertTrue(LocalMachine.isHost(host));
  }

  @Test
  void shouldRecogniseTheAddressOfANetworkInterfaceOfThisMachine() throws Exception {
    Optional<InetAddress> address = Collections.list(NetworkInterface.getNetworkInterfaces())
        .stream().flatMap(network -> Collections.list(network.getInetAddresses()).stream())
        .filter(candidate -> !candidate.isLoopbackAddress()).findFirst();
    assumeTrue(address.isPresent(), "This machine has no network interface address");

    assertTrue(LocalMachine.isAddress(address.get()));
    assertTrue(LocalMachine.isHost(address.get().getHostAddress()));
  }

  @ParameterizedTest
  @ValueSource(strings = {"192.0.2.1", "2001:db8::10", "[2001:db8::10]"})
  void shouldRefuseAddressOfAnotherMachine(String host) throws UnknownHostException {
    assertFalse(LocalMachine.isHost(host));
    assertFalse(LocalMachine.isAddress(InetAddress.getByName(host)));
  }

  @Test
  void shouldReportHostThatCannotBeResolved() {
    String host = "unresolvable.invalid";

    assertThrows(UnknownHostException.class, () -> LocalMachine.isHost(host));
  }
}
