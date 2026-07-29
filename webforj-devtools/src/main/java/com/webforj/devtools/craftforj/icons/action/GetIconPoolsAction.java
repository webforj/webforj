package com.webforj.devtools.craftforj.icons.action;

import com.google.gson.JsonObject;
import com.webforj.component.icons.DwcIcon;
import com.webforj.component.icons.FeatherIcon;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import java.util.Arrays;
import java.util.List;

/**
 * Action handler that lists the icon pools supported by the picker.
 *
 * <p>
 * Enum-backed pools (feather, dwc) list their names by reflecting over the icon enums on the
 * application classpath. The tabler pool carries no name list; the client loads its manifest.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class GetIconPoolsAction implements CraftforjActionHandler<GetIconPoolsAction.Response> {

  /** The action name. */
  public static final String ACTION = "icons.getPools";

  /**
   * {@inheritDoc}
   */
  @Override
  public String getAction() {
    return ACTION;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Response handle(JsonObject params) {
    List<PoolInfo> pools =
        List.of(new PoolInfo("tabler", null), new PoolInfo("feather", names(FeatherIcon.values())),
            new PoolInfo("dwc", names(DwcIcon.values())));

    return new Response(pools);
  }

  private static List<String> names(Object[] values) {
    return Arrays.stream(values).map(Object::toString).toList();
  }

  /**
   * Describes a single icon pool.
   */
  public static class PoolInfo {
    private final String name;
    private final List<String> names;

    PoolInfo(String name, List<String> names) {
      this.name = name;
      this.names = names;
    }

    /**
     * Gets the pool name.
     *
     * @return the pool name
     */
    public String getName() {
      return name;
    }

    /**
     * Gets the icon names, or null when the pool is not enum-backed.
     *
     * @return the icon names
     */
    public List<String> getNames() {
      return names;
    }
  }

  /**
   * Response containing the pools.
   */
  public static class Response {
    private final List<PoolInfo> pools;

    Response(List<PoolInfo> pools) {
      this.pools = pools;
    }

    /**
     * Gets the pools.
     *
     * @return the pools
     */
    public List<PoolInfo> getPools() {
      return pools;
    }
  }
}
