import { GenerationResult, UnifiedGenerationStats, GenerationType } from '@core/types'

export class StatsCollector {
	static collect(results: GenerationResult[]): UnifiedGenerationStats {
		const stats: UnifiedGenerationStats = {
			objectTypes: 0,
			enumTypes: 0,
			scalarTypes: 0,
			relationFields: 0,
			connectionTypes: 0,
			sortInputTypes: 0,
			filterInputTypes: 0,
		}

		for (const result of results) {
			switch (result.type) {
				case GenerationType.OBJECT:
					stats.objectTypes += result.count
					break
				case GenerationType.ENUM:
					stats.enumTypes += result.count
					break
				case GenerationType.SCALAR:
					stats.scalarTypes += result.count
					break
				case GenerationType.RELATION:
					stats.relationFields += result.count
					break
				case GenerationType.CONNECTION:
					stats.connectionTypes += result.count
					break
				case GenerationType.SORT:
					stats.sortInputTypes += result.count
					break
				case GenerationType.FILTER:
					stats.filterInputTypes += result.count
					break
			}
		}

		return stats
	}

}