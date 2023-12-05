package org.dwcj.component;

import com.google.gson.annotations.SerializedName;

/**
 * Supported expanses as defined by the Dwc Theme Engine.
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public enum Expanse implements ExpanseBase {
  /* The xlarge expanse as defined by the Dwc Theme Engine. */
  @SerializedName("xl")
  XLARGE,

  /* The large expanse as defined by the Dwc Theme Engine. */
  @SerializedName("l")
  LARGE,

  /* The medium expanse as defined by the Dwc Theme Engine. */
  @SerializedName("m")
  MEDIUM,

  /* The small expanse as defined by the Dwc Theme Engine. */
  @SerializedName("s")
  SMALL,

  /* The xsmall expanse as defined by the Dwc Theme Engine. */
  @SerializedName("xs")
  XSMALL,

  /* No expanse is applied. */
  @SerializedName("")
  NONE;
}
