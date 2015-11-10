public static class getTopologyInfo_result implements org.apache.thrift.TBase<getTopologyInfo_result, getTopologyInfo_result._Fields>, java.io.Serializable, Cloneable, Comparable<getTopologyInfo_result>   {
  boolean is_set_success();
  boolean is_set_e();
  boolean is_set_aze();
  int success;
  int e;
  int aze;

  public boolean equals(getTopologyInfo_result o1, getTopologyInfo_result o2) {
    boolean this_present_success = true && o1.is_set_success();
    boolean that_present_success = true && o2.is_set_success();
    if (this_present_success || that_present_success) {
      if (!(this_present_success && that_present_success)) {
        return false;
      }
      if (equals(o1.success, o2.success) == 0) {
        return false;
      }
    }

    boolean this_present_e = true && o1.is_set_e();
    boolean that_present_e = true && o2.is_set_e();
    if (this_present_e || that_present_e) {
      if (!(this_present_e && that_present_e)) {
        return false;
      }
      if (equals(o1.e, o2.e) == 0) {
        return false;
      }
    }

    boolean this_present_aze = true && o1.is_set_aze();
    boolean that_present_aze = true && o2.is_set_aze();
    if (this_present_aze || that_present_aze) {
      if (!(this_present_aze && that_present_aze)) {
        return false;
      }
      if (equals(o1.aze, o2.aze) == 0) {
        return false;
      }
    }

    return true;
  }
}