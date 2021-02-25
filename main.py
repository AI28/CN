from fortran_tangent import tangent
import matplotlib.pyplot as plt


random_values = tangent.generate_unbounded_values()
tangent_values = tangent.apply_optimal_tan(random_values)
fortran_tangent_values = tangent.apply_tan(random_values)


plt.close('all')
plt.plot(random_values, tangent_values)
plt.show()
