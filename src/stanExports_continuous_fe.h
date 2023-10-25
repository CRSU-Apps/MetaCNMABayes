// Generated by rstantools.  Do not edit by hand.

/*
    MetaCNMABayes is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MetaCNMABayes is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MetaCNMABayes.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef MODELS_HPP
#define MODELS_HPP
#define STAN__SERVICES__COMMAND_HPP
#ifndef USE_STANC3
#define USE_STANC3
#endif
#include <rstan/rstaninc.hpp>
// Code generated by stanc v2.26.1-4-gd72b68b7-dirty
#include <stan/model/model_header.hpp>
namespace model_continuous_fe_namespace {
inline void validate_positive_index(const char* var_name, const char* expr,
                                    int val) {
  if (val < 1) {
    std::stringstream msg;
    msg << "Found dimension size less than one in simplex declaration"
        << "; variable=" << var_name << "; dimension size expression=" << expr
        << "; expression value=" << val;
    std::string msg_str(msg.str());
    throw std::invalid_argument(msg_str.c_str());
  }
}
inline void validate_unit_vector_index(const char* var_name, const char* expr,
                                       int val) {
  if (val <= 1) {
    std::stringstream msg;
    if (val == 1) {
      msg << "Found dimension size one in unit vector declaration."
          << " One-dimensional unit vector is discrete"
          << " but the target distribution must be continuous."
          << " variable=" << var_name << "; dimension size expression=" << expr;
    } else {
      msg << "Found dimension size less than one in unit vector declaration"
          << "; variable=" << var_name << "; dimension size expression=" << expr
          << "; expression value=" << val;
    }
    std::string msg_str(msg.str());
    throw std::invalid_argument(msg_str.c_str());
  }
}
using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using std::pow;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::model_base_crtp;
using stan::model::rvalue;
using stan::model::cons_list;
using stan::model::index_uni;
using stan::model::index_max;
using stan::model::index_min;
using stan::model::index_min_max;
using stan::model::index_multi;
using stan::model::index_omni;
using stan::model::nil_index_list;
using namespace stan::math;
using stan::math::pow; 
stan::math::profile_map profiles__;
static int current_statement__= 0;
static const std::vector<string> locations_array__ = {" (found before start of program)",
                                                      " (in 'continuous_fe', line 19, column 2 to column 23)",
                                                      " (in 'continuous_fe', line 20, column 2 to column 25)",
                                                      " (in 'continuous_fe', line 24, column 29 to column 37)",
                                                      " (in 'continuous_fe', line 24, column 10 to column 21)",
                                                      " (in 'continuous_fe', line 24, column 2 to column 39)",
                                                      " (in 'continuous_fe', line 26, column 28 to column 36)",
                                                      " (in 'continuous_fe', line 26, column 10 to column 21)",
                                                      " (in 'continuous_fe', line 26, column 2 to column 38)",
                                                      " (in 'continuous_fe', line 28, column 14 to column 26)",
                                                      " (in 'continuous_fe', line 28, column 2 to column 29)",
                                                      " (in 'continuous_fe', line 30, column 4 to column 16)",
                                                      " (in 'continuous_fe', line 29, column 28 to line 31, column 3)",
                                                      " (in 'continuous_fe', line 29, column 2 to line 31, column 3)",
                                                      " (in 'continuous_fe', line 33, column 4 to column 27)",
                                                      " (in 'continuous_fe', line 32, column 28 to line 34, column 3)",
                                                      " (in 'continuous_fe', line 32, column 2 to line 34, column 3)",
                                                      " (in 'continuous_fe', line 36, column 4 to column 28)",
                                                      " (in 'continuous_fe', line 39, column 6 to column 61)",
                                                      " (in 'continuous_fe', line 40, column 6 to column 42)",
                                                      " (in 'continuous_fe', line 37, column 26 to line 41, column 5)",
                                                      " (in 'continuous_fe', line 37, column 4 to line 41, column 5)",
                                                      " (in 'continuous_fe', line 35, column 24 to line 42, column 3)",
                                                      " (in 'continuous_fe', line 35, column 2 to line 42, column 3)",
                                                      " (in 'continuous_fe', line 3, column 2 to column 15)",
                                                      " (in 'continuous_fe', line 5, column 13 to column 21)",
                                                      " (in 'continuous_fe', line 5, column 2 to column 23)",
                                                      " (in 'continuous_fe', line 7, column 2 to column 19)",
                                                      " (in 'continuous_fe', line 9, column 8 to column 16)",
                                                      " (in 'continuous_fe', line 9, column 18 to column 29)",
                                                      " (in 'continuous_fe', line 9, column 2 to column 31)",
                                                      " (in 'continuous_fe', line 11, column 8 to column 16)",
                                                      " (in 'continuous_fe', line 11, column 18 to column 29)",
                                                      " (in 'continuous_fe', line 11, column 2 to column 31)",
                                                      " (in 'continuous_fe', line 13, column 9 to column 17)",
                                                      " (in 'continuous_fe', line 13, column 19 to column 30)",
                                                      " (in 'continuous_fe', line 13, column 2 to column 32)",
                                                      " (in 'continuous_fe', line 15, column 18 to column 30)",
                                                      " (in 'continuous_fe', line 15, column 32 to column 40)",
                                                      " (in 'continuous_fe', line 15, column 42 to column 53)",
                                                      " (in 'continuous_fe', line 15, column 2 to column 55)",
                                                      " (in 'continuous_fe', line 19, column 10 to column 18)",
                                                      " (in 'continuous_fe', line 20, column 9 to column 21)"};
#include <stan_meta_header.hpp>
class model_continuous_fe final : public model_base_crtp<model_continuous_fe> {
private:
  int n_trials;
  std::vector<int> n_arms;
  int n_components;
  std::vector<std::vector<int>> n;
  std::vector<std::vector<int>> y;
  std::vector<std::vector<int>> sd;
  std::vector<std::vector<std::vector<double>>> components;
 
public:
  ~model_continuous_fe() { }
  
  inline std::string model_name() const final { return "model_continuous_fe"; }
  inline std::vector<std::string> model_compile_info() const noexcept {
    return std::vector<std::string>{"stanc_version = stanc3 v2.26.1-4-gd72b68b7-dirty", "stancflags = "};
  }
  
  
  model_continuous_fe(stan::io::var_context& context__,
                      unsigned int random_seed__ = 0,
                      std::ostream* pstream__ = nullptr) : model_base_crtp(0) {
    using local_scalar_t__ = double ;
    boost::ecuyer1988 base_rng__ = 
        stan::services::util::create_rng(random_seed__, 0);
    (void) base_rng__;  // suppress unused var warning
    static const char* function__ = "model_continuous_fe_namespace::model_continuous_fe";
    (void) function__;  // suppress unused var warning
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    (void) DUMMY_VAR__;  // suppress unused var warning
    
    try {
      int pos__;
      pos__ = std::numeric_limits<int>::min();
      
      pos__ = 1;
      current_statement__ = 24;
      context__.validate_dims("data initialization","n_trials","int",
          context__.to_vec());
      n_trials = std::numeric_limits<int>::min();
      
      current_statement__ = 24;
      n_trials = context__.vals_i("n_trials")[(1 - 1)];
      current_statement__ = 25;
      validate_non_negative_index("n_arms", "n_trials", n_trials);
      current_statement__ = 26;
      context__.validate_dims("data initialization","n_arms","int",
          context__.to_vec(n_trials));
      n_arms = std::vector<int>(n_trials, std::numeric_limits<int>::min());
      
      current_statement__ = 26;
      assign(n_arms, nil_index_list(), context__.vals_i("n_arms"),
        "assigning variable n_arms");
      current_statement__ = 27;
      context__.validate_dims("data initialization","n_components","int",
          context__.to_vec());
      n_components = std::numeric_limits<int>::min();
      
      current_statement__ = 27;
      n_components = context__.vals_i("n_components")[(1 - 1)];
      current_statement__ = 28;
      validate_non_negative_index("n", "n_trials", n_trials);
      current_statement__ = 29;
      validate_non_negative_index("n", "max(n_arms)", max(n_arms));
      current_statement__ = 30;
      context__.validate_dims("data initialization","n","int",
          context__.to_vec(n_trials, max(n_arms)));
      n = std::vector<std::vector<int>>(n_trials, std::vector<int>(max(
                                                                    n_arms), std::numeric_limits<int>::min()));
      
      {
        std::vector<int> n_flat__;
        current_statement__ = 30;
        assign(n_flat__, nil_index_list(), context__.vals_i("n"),
          "assigning variable n_flat__");
        current_statement__ = 30;
        pos__ = 1;
        current_statement__ = 30;
        for (int sym1__ = 1; sym1__ <= max(n_arms); ++sym1__) {
          current_statement__ = 30;
          for (int sym2__ = 1; sym2__ <= n_trials; ++sym2__) {
            current_statement__ = 30;
            assign(n,
              cons_list(index_uni(sym2__),
                cons_list(index_uni(sym1__), nil_index_list())),
              n_flat__[(pos__ - 1)], "assigning variable n");
            current_statement__ = 30;
            pos__ = (pos__ + 1);}}
      }
      current_statement__ = 31;
      validate_non_negative_index("y", "n_trials", n_trials);
      current_statement__ = 32;
      validate_non_negative_index("y", "max(n_arms)", max(n_arms));
      current_statement__ = 33;
      context__.validate_dims("data initialization","y","int",
          context__.to_vec(n_trials, max(n_arms)));
      y = std::vector<std::vector<int>>(n_trials, std::vector<int>(max(
                                                                    n_arms), std::numeric_limits<int>::min()));
      
      {
        std::vector<int> y_flat__;
        current_statement__ = 33;
        assign(y_flat__, nil_index_list(), context__.vals_i("y"),
          "assigning variable y_flat__");
        current_statement__ = 33;
        pos__ = 1;
        current_statement__ = 33;
        for (int sym1__ = 1; sym1__ <= max(n_arms); ++sym1__) {
          current_statement__ = 33;
          for (int sym2__ = 1; sym2__ <= n_trials; ++sym2__) {
            current_statement__ = 33;
            assign(y,
              cons_list(index_uni(sym2__),
                cons_list(index_uni(sym1__), nil_index_list())),
              y_flat__[(pos__ - 1)], "assigning variable y");
            current_statement__ = 33;
            pos__ = (pos__ + 1);}}
      }
      current_statement__ = 34;
      validate_non_negative_index("sd", "n_trials", n_trials);
      current_statement__ = 35;
      validate_non_negative_index("sd", "max(n_arms)", max(n_arms));
      current_statement__ = 36;
      context__.validate_dims("data initialization","sd","int",
          context__.to_vec(n_trials, max(n_arms)));
      sd = std::vector<std::vector<int>>(n_trials, std::vector<int>(max(
                                                                    n_arms), std::numeric_limits<int>::min()));
      
      {
        std::vector<int> sd_flat__;
        current_statement__ = 36;
        assign(sd_flat__, nil_index_list(), context__.vals_i("sd"),
          "assigning variable sd_flat__");
        current_statement__ = 36;
        pos__ = 1;
        current_statement__ = 36;
        for (int sym1__ = 1; sym1__ <= max(n_arms); ++sym1__) {
          current_statement__ = 36;
          for (int sym2__ = 1; sym2__ <= n_trials; ++sym2__) {
            current_statement__ = 36;
            assign(sd,
              cons_list(index_uni(sym2__),
                cons_list(index_uni(sym1__), nil_index_list())),
              sd_flat__[(pos__ - 1)], "assigning variable sd");
            current_statement__ = 36;
            pos__ = (pos__ + 1);}}
      }
      current_statement__ = 37;
      validate_non_negative_index("components", "n_components", n_components);
      current_statement__ = 38;
      validate_non_negative_index("components", "n_trials", n_trials);
      current_statement__ = 39;
      validate_non_negative_index("components", "max(n_arms)", max(n_arms));
      current_statement__ = 40;
      context__.validate_dims("data initialization","components","double",
          context__.to_vec(n_components, n_trials, max(n_arms)));
      components = std::vector<std::vector<std::vector<double>>>(n_components, std::vector<std::vector<double>>(n_trials, std::vector<double>(
        max(n_arms), std::numeric_limits<double>::quiet_NaN())));
      
      {
        std::vector<local_scalar_t__> components_flat__;
        current_statement__ = 40;
        assign(components_flat__, nil_index_list(),
          context__.vals_r("components"),
          "assigning variable components_flat__");
        current_statement__ = 40;
        pos__ = 1;
        current_statement__ = 40;
        for (int sym1__ = 1; sym1__ <= max(n_arms); ++sym1__) {
          current_statement__ = 40;
          for (int sym2__ = 1; sym2__ <= n_trials; ++sym2__) {
            current_statement__ = 40;
            for (int sym3__ = 1; sym3__ <= n_components; ++sym3__) {
              current_statement__ = 40;
              assign(components,
                cons_list(index_uni(sym3__),
                  cons_list(index_uni(sym2__),
                    cons_list(index_uni(sym1__), nil_index_list()))),
                components_flat__[(pos__ - 1)],
                "assigning variable components");
              current_statement__ = 40;
              pos__ = (pos__ + 1);}}}
      }
      current_statement__ = 41;
      validate_non_negative_index("mu", "n_trials", n_trials);
      current_statement__ = 42;
      validate_non_negative_index("d", "n_components", n_components);
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
    num_params_r__ = 0U;
    
    try {
      num_params_r__ += n_trials;
      num_params_r__ += n_components;
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
  }
  template <bool propto__, bool jacobian__, typename VecR, typename VecI, stan::require_vector_like_t<VecR>* = nullptr, stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr>
  inline stan::scalar_type_t<VecR> log_prob_impl(VecR& params_r__,
                                                 VecI& params_i__,
                                                 std::ostream* pstream__ = nullptr) const {
    using T__ = stan::scalar_type_t<VecR>;
    using local_scalar_t__ = T__;
    T__ lp__(0.0);
    stan::math::accumulator<T__> lp_accum__;
    static const char* function__ = "model_continuous_fe_namespace::log_prob";
(void) function__;  // suppress unused var warning
    stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    (void) DUMMY_VAR__;  // suppress unused var warning
    
    try {
      Eigen::Matrix<local_scalar_t__, -1, 1> mu;
      mu = Eigen::Matrix<local_scalar_t__, -1, 1>(n_trials);
      stan::math::fill(mu, DUMMY_VAR__);
      
      current_statement__ = 1;
      mu = in__.vector(n_trials);
      Eigen::Matrix<local_scalar_t__, -1, 1> d;
      d = Eigen::Matrix<local_scalar_t__, -1, 1>(n_components);
      stan::math::fill(d, DUMMY_VAR__);
      
      current_statement__ = 2;
      d = in__.vector(n_components);
      {
        current_statement__ = 3;
        validate_non_negative_index("theta", "n_trials", n_trials);
        current_statement__ = 4;
        validate_non_negative_index("theta", "max(n_arms)", max(n_arms));
        std::vector<Eigen::Matrix<local_scalar_t__, -1, 1>> theta;
        theta = std::vector<Eigen::Matrix<local_scalar_t__, -1, 1>>(n_trials, Eigen::Matrix<local_scalar_t__, -1, 1>(
          max(n_arms)));
        stan::math::fill(theta, DUMMY_VAR__);
        
        current_statement__ = 6;
        validate_non_negative_index("prec", "n_trials", n_trials);
        current_statement__ = 7;
        validate_non_negative_index("prec", "max(n_arms)", max(n_arms));
        std::vector<Eigen::Matrix<local_scalar_t__, -1, 1>> prec;
        prec = std::vector<Eigen::Matrix<local_scalar_t__, -1, 1>>(n_trials, Eigen::Matrix<local_scalar_t__, -1, 1>(
          max(n_arms)));
        stan::math::fill(prec, DUMMY_VAR__);
        
        current_statement__ = 9;
        validate_non_negative_index("D", "n_components", n_components);
        Eigen::Matrix<local_scalar_t__, 1, -1> D;
        D = Eigen::Matrix<local_scalar_t__, 1, -1>(n_components);
        stan::math::fill(D, DUMMY_VAR__);
        
        current_statement__ = 13;
        for (int i = 1; i <= n_components; ++i) {
          current_statement__ = 11;
          assign(D, cons_list(index_uni(i), nil_index_list()), d[(i - 1)],
            "assigning variable D");}
        current_statement__ = 16;
        for (int i = 1; i <= n_components; ++i) {
          current_statement__ = 14;
          lp_accum__.add(normal_lpdf<propto__>(d[(i - 1)], 0, 1000));}
        current_statement__ = 23;
        for (int i = 1; i <= n_trials; ++i) {
          current_statement__ = 17;
          lp_accum__.add(normal_lpdf<propto__>(mu[(i - 1)], 0, 1000));
          current_statement__ = 21;
          for (int k = 1; k <= n_arms[(i - 1)]; ++k) {
            current_statement__ = 18;
            assign(theta,
              cons_list(index_uni(i),
                cons_list(index_uni(k), nil_index_list())),
              (mu[(i - 1)] +
                multiply(D,
                  to_vector(
                    rvalue(components,
                      cons_list(index_omni(),
                        cons_list(index_uni(i),
                          cons_list(index_uni(k), nil_index_list()))),
                      "components")))), "assigning variable theta");
            current_statement__ = 19;
            lp_accum__.add(
              normal_lpdf<propto__>(y[(i - 1)][(k - 1)],
                theta[(i - 1)][(k - 1)], sd[(i - 1)][(k - 1)]));}}
      }
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
    lp_accum__.add(lp__);
    return lp_accum__.sum();
    } // log_prob_impl() 
    
  template <typename RNG, typename VecR, typename VecI, typename VecVar, stan::require_vector_like_vt<std::is_floating_point, VecR>* = nullptr, stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr, stan::require_std_vector_vt<std::is_floating_point, VecVar>* = nullptr>
  inline void write_array_impl(RNG& base_rng__, VecR& params_r__,
                               VecI& params_i__, VecVar& vars__,
                               const bool emit_transformed_parameters__ = true,
                               const bool emit_generated_quantities__ = true,
                               std::ostream* pstream__ = nullptr) const {
    using local_scalar_t__ = double;
    vars__.resize(0);
    stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
    static const char* function__ = "model_continuous_fe_namespace::write_array";
(void) function__;  // suppress unused var warning
    (void) function__;  // suppress unused var warning
    double lp__ = 0.0;
    (void) lp__;  // dummy to suppress unused var warning
    stan::math::accumulator<double> lp_accum__;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    (void) DUMMY_VAR__;  // suppress unused var warning
    
    try {
      Eigen::Matrix<double, -1, 1> mu;
      mu = Eigen::Matrix<double, -1, 1>(n_trials);
      stan::math::fill(mu, std::numeric_limits<double>::quiet_NaN());
      
      current_statement__ = 1;
      mu = in__.vector(n_trials);
      Eigen::Matrix<double, -1, 1> d;
      d = Eigen::Matrix<double, -1, 1>(n_components);
      stan::math::fill(d, std::numeric_limits<double>::quiet_NaN());
      
      current_statement__ = 2;
      d = in__.vector(n_components);
      for (int sym1__ = 1; sym1__ <= n_trials; ++sym1__) {
        vars__.emplace_back(mu[(sym1__ - 1)]);}
      for (int sym1__ = 1; sym1__ <= n_components; ++sym1__) {
        vars__.emplace_back(d[(sym1__ - 1)]);}
      if (logical_negation((primitive_value(emit_transformed_parameters__) ||
            primitive_value(emit_generated_quantities__)))) {
        return ;
      } 
      if (logical_negation(emit_generated_quantities__)) {
        return ;
      } 
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
    } // write_array_impl() 
    
  template <typename VecVar, typename VecI, stan::require_std_vector_t<VecVar>* = nullptr, stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr>
  inline void transform_inits_impl(const stan::io::var_context& context__,
                                   VecI& params_i__, VecVar& vars__,
                                   std::ostream* pstream__ = nullptr) const {
    using local_scalar_t__ = double;
    vars__.clear();
    vars__.reserve(num_params_r__);
    
    try {
      int pos__;
      pos__ = std::numeric_limits<int>::min();
      
      pos__ = 1;
      Eigen::Matrix<double, -1, 1> mu;
      mu = Eigen::Matrix<double, -1, 1>(n_trials);
      stan::math::fill(mu, std::numeric_limits<double>::quiet_NaN());
      
      {
        std::vector<local_scalar_t__> mu_flat__;
        current_statement__ = 1;
        assign(mu_flat__, nil_index_list(), context__.vals_r("mu"),
          "assigning variable mu_flat__");
        current_statement__ = 1;
        pos__ = 1;
        current_statement__ = 1;
        for (int sym1__ = 1; sym1__ <= n_trials; ++sym1__) {
          current_statement__ = 1;
          assign(mu, cons_list(index_uni(sym1__), nil_index_list()),
            mu_flat__[(pos__ - 1)], "assigning variable mu");
          current_statement__ = 1;
          pos__ = (pos__ + 1);}
      }
      Eigen::Matrix<double, -1, 1> d;
      d = Eigen::Matrix<double, -1, 1>(n_components);
      stan::math::fill(d, std::numeric_limits<double>::quiet_NaN());
      
      {
        std::vector<local_scalar_t__> d_flat__;
        current_statement__ = 2;
        assign(d_flat__, nil_index_list(), context__.vals_r("d"),
          "assigning variable d_flat__");
        current_statement__ = 2;
        pos__ = 1;
        current_statement__ = 2;
        for (int sym1__ = 1; sym1__ <= n_components; ++sym1__) {
          current_statement__ = 2;
          assign(d, cons_list(index_uni(sym1__), nil_index_list()),
            d_flat__[(pos__ - 1)], "assigning variable d");
          current_statement__ = 2;
          pos__ = (pos__ + 1);}
      }
      for (int sym1__ = 1; sym1__ <= n_trials; ++sym1__) {
        vars__.emplace_back(mu[(sym1__ - 1)]);}
      for (int sym1__ = 1; sym1__ <= n_components; ++sym1__) {
        vars__.emplace_back(d[(sym1__ - 1)]);}
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
    } // transform_inits_impl() 
    
  inline void get_param_names(std::vector<std::string>& names__) const {
    
    names__.clear();
    names__.emplace_back("mu");
    names__.emplace_back("d");
    } // get_param_names() 
    
  inline void get_dims(std::vector<std::vector<size_t>>& dimss__) const {
    dimss__.clear();
    dimss__.emplace_back(std::vector<size_t>{static_cast<size_t>(n_trials)});
    
    dimss__.emplace_back(std::vector<size_t>{
                                             static_cast<size_t>(n_components)});
    
    } // get_dims() 
    
  inline void constrained_param_names(
                                      std::vector<std::string>& param_names__,
                                      bool emit_transformed_parameters__ = true,
                                      bool emit_generated_quantities__ = true) const
    final {
    
    for (int sym1__ = 1; sym1__ <= n_trials; ++sym1__) {
      {
        param_names__.emplace_back(std::string() + "mu" + '.' + std::to_string(sym1__));
      }}
    for (int sym1__ = 1; sym1__ <= n_components; ++sym1__) {
      {
        param_names__.emplace_back(std::string() + "d" + '.' + std::to_string(sym1__));
      }}
    if (emit_transformed_parameters__) {
      
    }
    
    if (emit_generated_quantities__) {
      
    }
    
    } // constrained_param_names() 
    
  inline void unconstrained_param_names(
                                        std::vector<std::string>& param_names__,
                                        bool emit_transformed_parameters__ = true,
                                        bool emit_generated_quantities__ = true) const
    final {
    
    for (int sym1__ = 1; sym1__ <= n_trials; ++sym1__) {
      {
        param_names__.emplace_back(std::string() + "mu" + '.' + std::to_string(sym1__));
      }}
    for (int sym1__ = 1; sym1__ <= n_components; ++sym1__) {
      {
        param_names__.emplace_back(std::string() + "d" + '.' + std::to_string(sym1__));
      }}
    if (emit_transformed_parameters__) {
      
    }
    
    if (emit_generated_quantities__) {
      
    }
    
    } // unconstrained_param_names() 
    
  inline std::string get_constrained_sizedtypes() const {
    stringstream s__;
    s__ << "[{\"name\":\"mu\",\"type\":{\"name\":\"vector\",\"length\":" << n_trials << "},\"block\":\"parameters\"},{\"name\":\"d\",\"type\":{\"name\":\"vector\",\"length\":" << n_components << "},\"block\":\"parameters\"}]";
    return s__.str();
    } // get_constrained_sizedtypes() 
    
  inline std::string get_unconstrained_sizedtypes() const {
    stringstream s__;
    s__ << "[{\"name\":\"mu\",\"type\":{\"name\":\"vector\",\"length\":" << n_trials << "},\"block\":\"parameters\"},{\"name\":\"d\",\"type\":{\"name\":\"vector\",\"length\":" << n_components << "},\"block\":\"parameters\"}]";
    return s__.str();
    } // get_unconstrained_sizedtypes() 
    
  
    // Begin method overload boilerplate
    template <typename RNG>
    inline void write_array(RNG& base_rng,
                            Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                            Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                            const bool emit_transformed_parameters = true,
                            const bool emit_generated_quantities = true,
                            std::ostream* pstream = nullptr) const {
      std::vector<double> vars_vec(vars.size());
      std::vector<int> params_i;
      write_array_impl(base_rng, params_r, params_i, vars_vec,
          emit_transformed_parameters, emit_generated_quantities, pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i) {
        vars.coeffRef(i) = vars_vec[i];
      }
    }
    template <typename RNG>
    inline void write_array(RNG& base_rng, std::vector<double>& params_r,
                            std::vector<int>& params_i,
                            std::vector<double>& vars,
                            bool emit_transformed_parameters = true,
                            bool emit_generated_quantities = true,
                            std::ostream* pstream = nullptr) const {
      write_array_impl(base_rng, params_r, params_i, vars, emit_transformed_parameters, emit_generated_quantities, pstream);
    }
    template <bool propto__, bool jacobian__, typename T_>
    inline T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
                       std::ostream* pstream = nullptr) const {
      Eigen::Matrix<int, -1, 1> params_i;
      return log_prob_impl<propto__, jacobian__>(params_r, params_i, pstream);
    }
    template <bool propto__, bool jacobian__, typename T__>
    inline T__ log_prob(std::vector<T__>& params_r,
                        std::vector<int>& params_i,
                        std::ostream* pstream = nullptr) const {
      return log_prob_impl<propto__, jacobian__>(params_r, params_i, pstream);
    }
  
    inline void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double, Eigen::Dynamic, 1>& params_r,
                         std::ostream* pstream = nullptr) const final {
      std::vector<double> params_r_vec(params_r.size());
      std::vector<int> params_i;
      transform_inits_impl(context, params_i, params_r_vec, pstream);
      params_r.resize(params_r_vec.size());
      for (int i = 0; i < params_r.size(); ++i) {
        params_r.coeffRef(i) = params_r_vec[i];
      }
    }
    inline void transform_inits(const stan::io::var_context& context,
                                std::vector<int>& params_i,
                                std::vector<double>& vars,
                                std::ostream* pstream = nullptr) const final {
      transform_inits_impl(context, params_i, vars, pstream);
    }        
};
}
using stan_model = model_continuous_fe_namespace::model_continuous_fe;
#ifndef USING_R
// Boilerplate
stan::model::model_base& new_model(
        stan::io::var_context& data_context,
        unsigned int seed,
        std::ostream* msg_stream) {
  stan_model* m = new stan_model(data_context, seed, msg_stream);
  return *m;
}
stan::math::profile_map& get_stan_profile_data() {
  return model_continuous_fe_namespace::profiles__;
}
#endif
#endif