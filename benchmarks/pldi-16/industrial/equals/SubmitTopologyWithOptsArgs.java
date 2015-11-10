public static class submitTopologyWithOpts_args implements org.apache.thrift.TBase<submitTopologyWithOpts_args, submitTopologyWithOpts_args._Fields>, java.io.Serializable, Cloneable, Comparable<submitTopologyWithOpts_args>   {
  boolean is_set_name();
  boolean is_set_uploadedJarLocation();
  boolean is_set_jsonConf();
  boolean is_set_topology();
  boolean is_set_options();
  int name;
  int uploadedJarLocation;
  int jsonConf;
  int topology;
  int options;
  

  public boolean equals(submitTopologyWithOpts_args o1, submitTopologyWithOpts_args o2) {
    boolean this_present_name = true && o1.is_set_name();
    boolean that_present_name = true && o2.is_set_name();
    if (this_present_name || that_present_name) {
      if (!(this_present_name && that_present_name)) {
        return false;
      }
      if (equals(o1.name, o2.name) == 0) {
        return false;
      }
    }

    boolean this_present_uploadedJarLocation = true && o1.is_set_uploadedJarLocation();
    boolean that_present_uploadedJarLocation = true && o2.is_set_uploadedJarLocation();
    if (this_present_uploadedJarLocation || that_present_uploadedJarLocation) {
      if (!(this_present_uploadedJarLocation && that_present_uploadedJarLocation)) {
        return false;
      }
      if (equals(o1.uploadedJarLocation, o2.uploadedJarLocation) == 0) {
        return false;
      }
    }

    boolean this_present_jsonConf = true && o1.is_set_jsonConf();
    boolean that_present_jsonConf = true && o2.is_set_jsonConf();
    if (this_present_jsonConf || that_present_jsonConf) {
      if (!(this_present_jsonConf && that_present_jsonConf)) {
        return false;
      }
      if (equals(o1.jsonConf, o2.jsonConf) == 0) {
        return false;
      }
    }

    boolean this_present_topology = true && o1.is_set_topology();
    boolean that_present_topology = true && o2.is_set_topology();
    if (this_present_topology || that_present_topology) {
      if (!(this_present_topology && that_present_topology)) {
        return false;
      }
      if (equals(o1.topology, o2.topology) == 0) {
        return false;
      }
    }

    boolean this_present_options = true && o1.is_set_options();
    boolean that_present_options = true && o2.is_set_options();
    if (this_present_options || that_present_options) {
      if (!(this_present_options && that_present_options)) {
        return false;
      }
      if (equals(o1.options, o2.options) == 0) {
        return false;
      }
    }

    return true;
  }
}