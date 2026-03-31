// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::DataFrame pos_to_transactions_ternary_cpp(
    const Rcpp::CharacterVector& datetime,
    const Rcpp::NumericVector& open,
    const Rcpp::NumericVector& close,
    const Rcpp::IntegerVector& pos,
    const std::string mode = "new_open"  // "new_open" or "last_close"
) {
  const int n = pos.size();
  if (datetime.size() != n || open.size() != n || close.size() != n) {
    Rcpp::stop("datetime/open/close/pos must have the same length");
  }
  if (mode != "new_open" && mode != "last_close") {
    Rcpp::stop("mode must be 'new_open' or 'last_close'");
  }

  // Worst case: every bar flips => 2 tx per bar
  std::vector<std::string> out_dt;
  std::vector<std::string> out_action;   // "open" / "close"
  std::vector<std::string> out_dir;      // "long" / "short"
  std::vector<double>      out_size;     // always 1.0 here
  std::vector<double>      out_price;
  std::vector<int>         out_bar;      // bar index to preserve stable ordering
  std::vector<int>         out_seq;      // 0=close, 1=open within same bar

  out_dt.reserve(2 * n);
  out_action.reserve(2 * n);
  out_dir.reserve(2 * n);
  out_size.reserve(2 * n);
  out_price.reserve(2 * n);
  out_bar.reserve(2 * n);
  out_seq.reserve(2 * n);

  auto px_at = [&](int i) -> double {
    return (mode == "new_open") ? open[i] : close[i];
  };

  int prev = 0; // assume flat before first bar
  for (int i = 0; i < n; ++i) {
    int cur = pos[i];
    if (cur != -1 && cur != 0 && cur != 1) {
      Rcpp::stop("pos must be in {-1, 0, 1} for ternary version");
    }

    // 1) CLOSE first (important for flips)
    if (prev ==  1 && cur !=  1) {
      out_dt.push_back(Rcpp::as<std::string>(datetime[i]));
      out_action.push_back("close");
      out_dir.push_back("long");
      out_size.push_back(1.0);
      out_price.push_back(px_at(i));
      out_bar.push_back(i);
      out_seq.push_back(0);
    } else if (prev == -1 && cur != -1) {
      out_dt.push_back(Rcpp::as<std::string>(datetime[i]));
      out_action.push_back("close");
      out_dir.push_back("short");
      out_size.push_back(1.0);
      out_price.push_back(px_at(i));
      out_bar.push_back(i);
      out_seq.push_back(0);
    }

    // 2) OPEN second
    if (cur ==  1 && prev !=  1) {
      out_dt.push_back(Rcpp::as<std::string>(datetime[i]));
      out_action.push_back("open");
      out_dir.push_back("long");
      out_size.push_back(1.0);
      out_price.push_back(px_at(i));
      out_bar.push_back(i);
      out_seq.push_back(1);
    } else if (cur == -1 && prev != -1) {
      out_dt.push_back(Rcpp::as<std::string>(datetime[i]));
      out_action.push_back("open");
      out_dir.push_back("short");
      out_size.push_back(1.0);
      out_price.push_back(px_at(i));
      out_bar.push_back(i);
      out_seq.push_back(1);
    }

    prev = cur;
  }

  return Rcpp::DataFrame::create(
    Rcpp::_["datetime"] = out_dt,
    Rcpp::_["action"]   = out_action,
    Rcpp::_["direction"]= out_dir,
    Rcpp::_["size"]     = out_size,
    Rcpp::_["price"]    = out_price,
    Rcpp::_["bar_index"]= out_bar,
    Rcpp::_["seq"]      = out_seq,
    Rcpp::_["stringsAsFactors"] = false
  );
}
