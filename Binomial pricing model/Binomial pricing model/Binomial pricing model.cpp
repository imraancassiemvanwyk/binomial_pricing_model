// Binomial pricing model.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include <iostream>
#include <vector>
#include <iomanip>
#include <limits>

class binomial 
{
public:
    double stock_price;
    long double up_factor;
    long double down_factor;
    double p;
    int steps;
    double volaitlity;
    double dividends;
    double time_to_maturity;
    double strike_price;
    double risk_free_rate;
    std::vector<std::vector<double>> tree;


    double d1()
    {
        return ((log(stock_price / strike_price) +
            (time_to_maturity * (risk_free_rate - dividends + (0.5 * volaitlity * volaitlity))))
            / (volaitlity * std::sqrt(time_to_maturity)));
    }

    double d2()
    {
        return (d1() - (volaitlity * std::sqrt(time_to_maturity)));
    }
    double peizer_pratt_inversion(double x) 
    {
        int sign = 0;
        if (x>0)
        {
            sign = 1;
        }
        else if (x < 0) 
        {
            sign = -1;
        }
        double term1 = (x / (steps+(1/3)+(0.1/steps+1)))* (x / (steps + (1 / 3) + (0.1 / steps + 1)));
        return 0.5 + (0.5 * sign) * std::sqrt(1 - std::exp(-1 * term1 * (steps + (1 / 6))));
    }

    double single_branch(double up_branch, double down_branch) 
    {
        return (std::exp(-1 * risk_free_rate * (time_to_maturity / steps)) * ((p * up_branch) + ((1 - p) * down_branch)));
    }

    void populate_tree() 
    {
        tree.resize(steps);             
        for (size_t i = 0; i < steps; i++)
        {
            tree[i].resize(std::pow(2,i));
        }
        tree[0][0] = stock_price;
        for (size_t i = 1; i < steps; i++)
        {
            for (size_t j = 0; j < tree[i-1].size(); j++)
            {
                tree[i][(j * 2)] = up_factor * tree[i - 1][j];
                tree[i][(j * 2) + 1] = down_factor * tree[i - 1][j];;
            }
        }
    }

    double euro_call() 
    {

        p = peizer_pratt_inversion(d2());
        double common = std::exp((risk_free_rate - dividends) * (time_to_maturity / steps));
        up_factor = common * (peizer_pratt_inversion(d1()) / p);
        down_factor = common * ((1 - peizer_pratt_inversion(d1())) / (1 - p));

        populate_tree();

        for (size_t i = 0; i < tree[tree.size()-1].size(); i++)
        {
            tree[tree.size()-1][i] = std::max(0.0, tree[tree.size()-1][i] - strike_price);
        }

        std::reverse(tree.begin(), tree.end());
        for (size_t i = 1; i < steps; i++)
        {
            for (size_t j = 0; j < tree[i].size(); j++)
            {
                tree[i][j] = single_branch(tree[i - 1][j * 2], tree[i - 1][(j * 2) + 1]);
            }
        }
        return tree[tree.size()-1][0];
    }

    double euro_put()
    {
        p = peizer_pratt_inversion(d2());
        double common = std::exp((risk_free_rate - dividends) * (time_to_maturity / steps));
        up_factor = common * (peizer_pratt_inversion(d1()) / p);
        down_factor = common * ((1 - peizer_pratt_inversion(d1())) / (1 - p));

        populate_tree();

        for (size_t i = 0; i < tree[tree.size() - 1].size(); i++)
        {
            tree[tree.size() - 1][i] = std::max(0.0, strike_price - tree[tree.size() - 1][i]);
        }

        std::reverse(tree.begin(), tree.end());
        for (size_t i = 1; i < steps; i++)
        {
            for (size_t j = 0; j < tree[i].size(); j++)
            {
                tree[i][j] = single_branch(tree[i - 1][j * 2], tree[i - 1][(j * 2) + 1]);
            }
        }
        return tree[tree.size() - 1][0];
    }
    double american_call()
    {
        p = peizer_pratt_inversion(d2());
        double common = std::exp((risk_free_rate - dividends) * (time_to_maturity / steps));
        up_factor = common * (peizer_pratt_inversion(d1()) / p);
        down_factor = common * ((1 - peizer_pratt_inversion(d1())) / (1 - p));

        populate_tree();
        
        for (size_t i = 0; i < tree[tree.size() - 1].size(); i++)
        {
            tree[tree.size() - 1][i] = std::max(0.0, tree[tree.size() - 1][i] - strike_price);
        }

        std::reverse(tree.begin(), tree.end());
        for (size_t i = 1; i < steps; i++)
        {
            for (size_t j = 0; j < tree[i].size(); j++)
            {
                tree[i][j] = std::max(std::max(tree[i][j]-strike_price,0.0)
                    , single_branch(tree[i - 1][j * 2], tree[i - 1][(j * 2) + 1]));
            }
        }
        return tree[tree.size() - 1][0];
    }

    double american_put()
    {
        p = peizer_pratt_inversion(d2());
        double common = std::exp((risk_free_rate - dividends) * (time_to_maturity / steps));
        up_factor = common * (peizer_pratt_inversion(d1()) / p);
        down_factor = common * ((1 - peizer_pratt_inversion(d1())) / (1 - p));

        populate_tree();

        for (size_t i = 0; i < tree[tree.size() - 1].size(); i++)
        {
            tree[tree.size() - 1][i] = std::max(0.0, strike_price - tree[tree.size() - 1][i]);
        }

        std::reverse(tree.begin(), tree.end());
        for (size_t i = 1; i < steps; i++)
        {
            for (size_t j = 0; j < tree[i].size(); j++)
            {
                tree[i][j] = tree[i][j] = std::max(strike_price - std::max(tree[i][j] , 0.0)
                    , single_branch(tree[i - 1][j * 2], tree[i - 1][(j * 2) + 1]));
            }
        }
        return tree[tree.size() - 1][0];
    }



};




int main()
{
    binomial b;
    b.steps=15;
    b.volaitlity=0.07;
    b.dividends=0.03;
    b.time_to_maturity=1;
    b.strike_price=50;
    b.risk_free_rate=0.02;
    b.stock_price = 120;
    double x = b.american_put();
    std::cout  << x;
}

// Run program: Ctrl + F5 or Debug > Start Without Debugging menu
// Debug program: F5 or Debug > Start Debugging menu

// Tips for Getting Started: 
//   1. Use the Solution Explorer window to add/manage files
//   2. Use the Team Explorer window to connect to source control
//   3. Use the Output window to see build output and other messages
//   4. Use the Error List window to view errors
//   5. Go to Project > Add New Item to create new code files, or Project > Add Existing Item to add existing code files to the project
//   6. In the future, to open this project again, go to File > Open > Project and select the .sln file
