import { FileWriter } from '@utils/file-writer'
import { OutputFormat } from '@utils/constants'
import path from 'path'

export class HelperFileWriter {
	private fileWriter = new FileWriter()

	async writeHelperFiles(_outputFormat: OutputFormat, outputPath: string, helperCode?: string): Promise<string[]> {
		const files: string[] = []

		// Write helper code if provided (always as TypeScript)
		if (helperCode) {
			const helperPath = this.resolveHelperPath(outputPath)
			// Combine GraphQL field selection utility with helper code in one file
			const completeHelperCode = this.combineHelperCode(helperCode)
			await this.fileWriter.write(completeHelperCode, helperPath, 'Helper utilities')
			files.push(helperPath)
		}

		return files
	}

	private resolveHelperPath(basePath: string): string {
		const baseDir = path.dirname(basePath)
		const baseName = path.basename(basePath, path.extname(basePath))
		
		// Always generate TypeScript helpers
		return path.join(baseDir, `${baseName}-helpers.ts`)
	}

	private combineHelperCode(helperCode: string): string {
		// No longer combining with field selection code - it's handled by templates
		return helperCode
	}

}