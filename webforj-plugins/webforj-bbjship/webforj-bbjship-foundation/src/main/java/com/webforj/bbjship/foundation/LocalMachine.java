package com.webforj.bbjship.foundation;

import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.net.UnknownHostException;

/**
 * Tells whether an address or a host name belongs to this machine.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class LocalMachine {

  private LocalMachine() {}

  /**
   * Checks whether an address is a loopback address or the address of a network interface of this
   * machine.
   *
   * @param address the address
   *
   * @return true when the address belongs to this machine
   */
  public static boolean isAddress(InetAddress address) {
    if (address.isLoopbackAddress()) {
      return true;
    }

    try {
      return NetworkInterface.getByInetAddress(address) != null;
    } catch (SocketException unavailable) {
      return false;
    }
  }

  /**
   * Checks whether every address a host name resolves to belongs to this machine.
   *
   * @param host the host name or address literal
   *
   * @return true when the host belongs to this machine
   * @throws UnknownHostException if the host cannot be resolved
   */
  public static boolean isHost(String host) throws UnknownHostException {
    for (InetAddress address : InetAddress.getAllByName(host)) {
      if (!isAddress(address)) {
        return false;
      }
    }

    return true;
  }
}
